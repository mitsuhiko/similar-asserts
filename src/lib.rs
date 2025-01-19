//! `similar-asserts` is a crate that enhances the default assertion
//! experience by using [similar](https://crates.io/crates/similar) for diffing.
//! On failed assertions it renders out a colorized diff to the terminal.
//!
//! It comes with a handful of macros to replace [`std::assert_eq!`] with:
//!
//! - [`assert_eq!`]: diffs `Debug` on assertion failure.
#![cfg_attr(
    feature = "serde",
    doc = r#"
- [`assert_serde_eq!`]: diffs `Serialize` on assertion failure.
"#
)]
//!
//! ![](https://raw.githubusercontent.com/mitsuhiko/similar-asserts/main/assets/screenshot.png)
//!
//! # Usage
//!
//! ```rust
//! use similar_asserts::assert_eq;
//! assert_eq!((1..3).collect::<Vec<_>>(), vec![1, 2]);
//! ```
//!
//! Optionally the assertion macros also let you "name" the left and right
//! side which will produce slightly more explicit output:
//!
//! ```rust
//! use similar_asserts::assert_eq;
//! assert_eq!(expected: vec![1, 2], actual: (1..3).collect::<Vec<_>>());
//! ```
//!
//! # Feature Flags
//!
//! * `unicode` enable improved character matching (enabled by default)
//! * `serde` turns on support for serde.
//!
//! # Faster Builds
//!
//! This crate works best if you add it as `dev-dependency` only.  To make your code
//! still compile you can use conditional uses that override the default uses for the
//! `assert_eq!` macro from the stdlib:
//!
//! ```
//! #[cfg(test)]
//! use similar_asserts::assert_eq;
//! ```
//!
//! Since `similar_asserts` uses the `similar` library for diffing you can also
//! enable optimziation for them in all build types for quicker diffing.  Add
//! this to your `Cargo.toml`:
//!
//! ```toml
//! [profile.dev.package.similar]
//! opt-level = 3
//! ```
//!
//! # String Truncation
//!
//! By default the assertion only shows 200 characters.  This can be changed with the
//! `SIMILAR_ASSERTS_MAX_STRING_LENGTH` environment variable.  Setting it to `0` disables
//! all truncation, otherwise it sets the maximum number of characters before truncation
//! kicks in.
//!
//! # Manual Diff Printing
//!
//! If you want to build your own comparison macros and you need a quick and simple
//! way to render diffs, you can use the [`SimpleDiff`] type and display it:
//!
//! ```should_panic
//! use similar_asserts::SimpleDiff;
//! panic!("Not equal\n\n{}", SimpleDiff::from_str("a\nb\n", "b\nb\n", "left", "right"));
//! ```
use std::borrow::Cow;
use std::fmt::{self, Display};
use std::time::Duration;

use console::{style, Style};
use similar::{Algorithm, ChangeTag, TextDiff};

#[cfg(feature = "serde")]
#[doc(hidden)]
pub mod serde_impl;

// This needs to be public as we are using it internally in a macro.
#[doc(hidden)]
pub mod print;

/// The maximum number of characters a string can be long before truncating.
fn get_max_string_length() -> usize {
    use std::sync::atomic::{AtomicUsize, Ordering};
    static TRUNCATE: AtomicUsize = AtomicUsize::new(!0);
    let rv = TRUNCATE.load(Ordering::Relaxed);
    if rv != !0 {
        return rv;
    }
    let rv: usize = std::env::var("SIMILAR_ASSERTS_MAX_STRING_LENGTH")
        .ok()
        .and_then(|x| x.parse().ok())
        .unwrap_or(200);
    TRUNCATE.store(rv, Ordering::Relaxed);
    rv
}

/// A console printable diff.
///
/// The [`Display`](std::fmt::Display) implementation of this type renders out a
/// diff with ANSI markers so it creates a nice colored diff. This can be used to
/// build your own custom assertions in addition to the ones from this crate.
///
/// It does not provide much customization beyond what's possible done by default.
pub struct SimpleDiff<'a> {
    pub(crate) left_short: Cow<'a, str>,
    pub(crate) right_short: Cow<'a, str>,
    pub(crate) left_expanded: Option<Cow<'a, str>>,
    pub(crate) right_expanded: Option<Cow<'a, str>>,
    pub(crate) left_label: &'a str,
    pub(crate) right_label: &'a str,
}

impl<'a> SimpleDiff<'a> {
    /// Creates a diff from two strings.
    ///
    /// `left_label` and `right_label` are the labels used for the two sides.
    /// `"left"` and `"right"` are sensible defaults if you don't know what
    /// to pick.
    pub fn from_str(
        left: &'a str,
        right: &'a str,
        left_label: &'a str,
        right_label: &'a str,
    ) -> SimpleDiff<'a> {
        SimpleDiff {
            left_short: left.into(),
            right_short: right.into(),
            left_expanded: None,
            right_expanded: None,
            left_label,
            right_label,
        }
    }

    #[doc(hidden)]
    pub fn __from_macro(
        left_short: Option<Cow<'a, str>>,
        right_short: Option<Cow<'a, str>>,
        left_expanded: Option<Cow<'a, str>>,
        right_expanded: Option<Cow<'a, str>>,
        left_label: &'a str,
        right_label: &'a str,
    ) -> SimpleDiff<'a> {
        SimpleDiff {
            left_short: left_short.unwrap_or_else(|| "<unprintable object>".into()),
            right_short: right_short.unwrap_or_else(|| "<unprintable object>".into()),
            left_expanded,
            right_expanded,
            left_label,
            right_label,
        }
    }

    /// Returns the left side as string.
    fn left(&self) -> &str {
        self.left_expanded.as_deref().unwrap_or(&self.left_short)
    }

    /// Returns the right side as string.
    fn right(&self) -> &str {
        self.right_expanded.as_deref().unwrap_or(&self.right_short)
    }

    /// Returns the label padding
    fn label_padding(&self) -> usize {
        self.left_label
            .chars()
            .count()
            .max(self.right_label.chars().count())
    }

    #[doc(hidden)]
    #[track_caller]
    pub fn fail_assertion(&self, hint: &dyn Display) {
        // prefer the shortened version here.
        let len = get_max_string_length();
        let (left, left_truncated) = truncate_str(&self.left_short, len);
        let (right, right_truncated) = truncate_str(&self.right_short, len);

        panic!(
            "assertion failed: `({} == {})`{}'\
               \n {:>label_padding$}: `{:?}`{}\
               \n {:>label_padding$}: `{:?}`{}\
               \n\n{}\n",
            self.left_label,
            self.right_label,
            hint,
            self.left_label,
            DebugStrTruncated(left, left_truncated),
            if left_truncated { " (truncated)" } else { "" },
            self.right_label,
            DebugStrTruncated(right, right_truncated),
            if right_truncated { " (truncated)" } else { "" },
            &self,
            label_padding = self.label_padding(),
        );
    }
}

fn truncate_str(s: &str, chars: usize) -> (&str, bool) {
    if chars == 0 {
        return (s, false);
    }
    s.char_indices()
        .enumerate()
        .find_map(|(idx, (offset, _))| {
            if idx == chars {
                Some((&s[..offset], true))
            } else {
                None
            }
        })
        .unwrap_or((s, false))
}

struct DebugStrTruncated<'s>(&'s str, bool);

impl fmt::Debug for DebugStrTruncated<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.1 {
            let s = format!("{}...", self.0);
            fmt::Debug::fmt(&s, f)
        } else {
            fmt::Debug::fmt(&self.0, f)
        }
    }
}

fn trailing_newline(s: &str) -> &str {
    if s.ends_with("\r\n") {
        "\r\n"
    } else if s.ends_with("\r") {
        "\r"
    } else if s.ends_with("\n") {
        "\n"
    } else {
        ""
    }
}

fn detect_newlines(s: &str) -> (bool, bool, bool) {
    let mut last_char = None;
    let mut detected_crlf = false;
    let mut detected_cr = false;
    let mut detected_lf = false;

    for c in s.chars() {
        if c == '\n' {
            if last_char.take() == Some('\r') {
                detected_crlf = true;
            } else {
                detected_lf = true;
            }
        }
        if last_char == Some('\r') {
            detected_cr = true;
        }
        last_char = Some(c);
    }
    if last_char == Some('\r') {
        detected_cr = true;
    }

    (detected_cr, detected_crlf, detected_lf)
}

#[allow(clippy::match_like_matches_macro)]
fn newlines_matter(left: &str, right: &str) -> bool {
    if trailing_newline(left) != trailing_newline(right) {
        return true;
    }

    let (cr1, crlf1, lf1) = detect_newlines(left);
    let (cr2, crlf2, lf2) = detect_newlines(right);

    match (cr1 || cr2, crlf1 || crlf2, lf1 || lf2) {
        (false, false, false) => false,
        (true, false, false) => false,
        (false, true, false) => false,
        (false, false, true) => false,
        _ => true,
    }
}

impl fmt::Display for SimpleDiff<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let left = self.left();
        let right = self.right();
        let newlines_matter = newlines_matter(left, right);

        if left == right {
            writeln!(
                f,
                "{}: the two values are the same in string form.",
                style("Invisible differences").bold(),
            )?;
            return Ok(());
        }

        let diff = TextDiff::configure()
            .timeout(Duration::from_millis(200))
            .algorithm(Algorithm::Patience)
            .diff_lines(left, right);

        writeln!(
            f,
            "{} ({}{}|{}{}):",
            style("Differences").bold(),
            style("-").red().dim(),
            style(self.left_label).red(),
            style("+").green().dim(),
            style(self.right_label).green(),
        )?;
        for (idx, group) in diff.grouped_ops(4).into_iter().enumerate() {
            if idx > 0 {
                writeln!(f, "@ {}", style("~~~").dim())?;
            }
            for op in group {
                for change in diff.iter_inline_changes(&op) {
                    let (marker, style) = match change.tag() {
                        ChangeTag::Delete => ('-', Style::new().red()),
                        ChangeTag::Insert => ('+', Style::new().green()),
                        ChangeTag::Equal => (' ', Style::new().dim()),
                    };
                    write!(f, "{}", style.apply_to(marker).dim().bold())?;
                    for &(emphasized, value) in change.values() {
                        let value = if newlines_matter {
                            Cow::Owned(
                                value
                                    .replace("\r", "␍\r")
                                    .replace("\n", "␊\n")
                                    .replace("␍\r␊\n", "␍␊\r\n"),
                            )
                        } else {
                            Cow::Borrowed(value)
                        };
                        if emphasized {
                            write!(f, "{}", style.clone().underlined().bold().apply_to(value))?;
                        } else {
                            write!(f, "{}", style.apply_to(value))?;
                        }
                    }
                    if change.missing_newline() {
                        writeln!(f)?;
                    }
                }
            }
        }

        Ok(())
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __assert_eq {
    (
        $method:ident,
        $left_label:ident,
        $left:expr,
        $right_label:ident,
        $right:expr,
        $hint_suffix:expr
    ) => {{
        match (&($left), &($right)) {
            (left_val, right_val) =>
            {
                #[allow(unused_mut)]
                if !(*left_val == *right_val) {
                    use $crate::print::{PrintMode, PrintObject};
                    let left_label = stringify!($left_label);
                    let right_label = stringify!($right_label);
                    let mut left_val_tup1 = (&left_val,);
                    let mut right_val_tup1 = (&right_val,);
                    let mut left_val_tup2 = (&left_val,);
                    let mut right_val_tup2 = (&right_val,);
                    let left_short = left_val_tup1.print_object(PrintMode::Default);
                    let right_short = right_val_tup1.print_object(PrintMode::Default);
                    let left_expanded = left_val_tup2.print_object(PrintMode::Expanded);
                    let right_expanded = right_val_tup2.print_object(PrintMode::Expanded);
                    let diff = $crate::SimpleDiff::__from_macro(
                        left_short,
                        right_short,
                        left_expanded,
                        right_expanded,
                        left_label,
                        right_label,
                    );
                    diff.fail_assertion(&$hint_suffix);
                }
            }
        }
    }};
}

/// Asserts that two expressions are equal to each other (using [`PartialEq`]).
///
/// On panic, this macro will print the values of the expressions with their
/// [`Debug`] or [`ToString`] representations with a colorized diff of the
/// changes in the debug output.  It picks [`Debug`] for all types that are
/// not strings themselves and [`ToString`] for [`str`] and [`String`].
///
/// Like [`assert!`], this macro has a second form, where a custom panic
/// message can be provided.
///
/// ```rust
/// use similar_asserts::assert_eq;
/// assert_eq!((1..3).collect::<Vec<_>>(), vec![1, 2]);
/// ```
#[macro_export]
macro_rules! assert_eq {
    ($left_label:ident: $left:expr, $right_label:ident: $right:expr $(,)?) => ({
        $crate::__assert_eq!(make_diff, $left_label, $left, $right_label, $right, "");
    });
    ($left_label:ident: $left:expr, $right_label:ident: $right:expr, $($arg:tt)*) => ({
        $crate::__assert_eq!(make_diff, $left_label, $left, $right_label, $right, format_args!(": {}", format_args!($($arg)*)));
    });
    ($left:expr, $right:expr $(,)?) => ({
        $crate::assert_eq!(left: $left, right: $right);
    });
    ($left:expr, $right:expr, $($arg:tt)*) => ({
        $crate::assert_eq!(left: $left, right: $right, $($arg)*);
    });
}

/// Deprecated macro.  Use [`assert_eq!`] instead.
#[macro_export]
#[doc(hidden)]
#[deprecated(since = "1.4.0", note = "use assert_eq! instead")]
macro_rules! assert_str_eq {
    ($left_label:ident: $left:expr, $right_label:ident: $right:expr $(,)?) => ({
        $crate::assert_eq!($left_label: $left, $right_label: $right);
    });
    ($left_label:ident: $left:expr, $right_label:ident: $right:expr, $($arg:tt)*) => ({
        $crate::assert_eq!($left_label: $left, $right_label: $right, $($arg)*);
    });
    ($left:expr, $right:expr $(,)?) => ({
        $crate::assert_eq!($left, $right);
    });
    ($left:expr, $right:expr, $($arg:tt)*) => ({
        $crate::assert_eq!($left, $right, $($arg)*);
    });
}

#[test]
fn test_newlines_matter() {
    assert!(newlines_matter("\r\n", "\n"));
    assert!(newlines_matter("foo\n", "foo"));
    assert!(newlines_matter("foo\r\nbar", "foo\rbar"));
    assert!(newlines_matter("foo\r\nbar", "foo\nbar"));
    assert!(newlines_matter("foo\r\nbar\n", "foobar"));
    assert!(newlines_matter("foo\nbar\r\n", "foo\nbar\r\n"));
    assert!(newlines_matter("foo\nbar\n", "foo\nbar"));

    assert!(!newlines_matter("foo\nbar", "foo\nbar"));
    assert!(!newlines_matter("foo\nbar\n", "foo\nbar\n"));
    assert!(!newlines_matter("foo\r\nbar", "foo\r\nbar"));
    assert!(!newlines_matter("foo\r\nbar\r\n", "foo\r\nbar\r\n"));
    assert!(!newlines_matter("foo\r\nbar", "foo\r\nbar"));
}

#[test]
fn test_truncate_str() {
    assert_eq!(truncate_str("foobar", 20), ("foobar", false));
    assert_eq!(truncate_str("foobar", 2), ("fo", true));
    assert_eq!(truncate_str("🔥🔥🔥🔥🔥", 2), ("🔥🔥", true));
}
