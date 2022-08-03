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
use std::fmt;
use std::time::Duration;

use console::{style, Style};
use similar::{Algorithm, ChangeTag, TextDiff};

#[cfg(feature = "serde")]
mod serde_impl;

/// A console printable diff.
///
/// The [`Display`](std::fmt::Display) implementation of this type renders out a
/// diff with ANSI markers so it creates a nice colored diff. This can be used to
/// build your own custom assertions in addition to the ones from this crate.
///
/// It does not provide much customization beyond what's possible done by default.
pub struct SimpleDiff<'a> {
    pub(crate) left: Cow<'a, str>,
    pub(crate) right: Cow<'a, str>,
    pub(crate) left_short: Option<Cow<'a, str>>,
    pub(crate) right_short: Option<Cow<'a, str>>,
    pub(crate) left_label: &'static str,
    pub(crate) right_label: &'static str,
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
        left_label: &'static str,
        right_label: &'static str,
    ) -> SimpleDiff<'a> {
        SimpleDiff {
            left: left.into(),
            right: right.into(),
            left_short: None,
            right_short: None,
            left_label,
            right_label,
        }
    }

    /// Returns the left side as string.
    #[doc(hidden)]
    pub fn _private_left(&self) -> &str {
        self.left_short.as_deref().unwrap_or(&self.left)
    }

    /// Returns the right side as string.
    #[doc(hidden)]
    pub fn _private_right(&self) -> &str {
        self.right_short.as_deref().unwrap_or(&self.right)
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

impl<'a> fmt::Display for SimpleDiff<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let newlines_matter = newlines_matter(&self.left, &self.right);

        if self.left == self.right {
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
            .diff_lines(&self.left, &self.right);

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

/// This hidden module is used by the macro system to figure out which
/// implementation of the string representation to use.  The main trait is
/// `MakeDiff` which has two primary implementations: One for `UsesFromStr`
/// implementing types (strings) and one for all debuggable types.
///
/// Additionally for the serde macro there is a `MakeSerdeDiff`.  This uses
/// the automatic deref system of Rust to disambiugate between strings and
/// non strings.
#[doc(hidden)]
pub mod traits {
    use std::borrow::Cow;
    use std::fmt::Debug;

    pub trait UsesFromStr: AsRef<str> {}

    impl UsesFromStr for str {}
    impl UsesFromStr for String {}
    impl<'a> UsesFromStr for Cow<'a, str> {}
    impl<T: UsesFromStr + ?Sized> UsesFromStr for &T {}

    pub trait MakeDiff<'a> {
        fn make_diff(
            self,
            left_label: &'static str,
            right_label: &'static str,
        ) -> crate::SimpleDiff<'a>
        where
            Self: 'a;
    }

    impl<'a, T: Debug, U: Debug> MakeDiff<'a> for &'a (T, U) {
        fn make_diff(
            self,
            left_label: &'static str,
            right_label: &'static str,
        ) -> crate::SimpleDiff<'a>
        where
            Self: 'a,
        {
            let left = &self.0;
            let right = &self.1;
            let left_short = Some(format!("{:?}", left).into());
            let right_short = Some(format!("{:?}", right).into());
            let left = format!("{:#?}", left).into();
            let right = format!("{:#?}", right).into();
            crate::SimpleDiff {
                left,
                right,
                left_short,
                right_short,
                left_label,
                right_label,
            }
        }
    }

    impl<'a, T, U> MakeDiff<'a> for (&'a T, &'a U)
    where
        T: UsesFromStr + ?Sized,
        U: UsesFromStr + ?Sized,
    {
        fn make_diff(
            self,
            left_label: &'static str,
            right_label: &'static str,
        ) -> crate::SimpleDiff<'a>
        where
            Self: 'a,
        {
            crate::SimpleDiff::from_str(self.0.as_ref(), self.1.as_ref(), left_label, right_label)
        }
    }

    // if serde is compiled in, we also need `MakeSerdeDiff`.
    #[cfg(feature = "serde")]
    pub use super::serde_impl::MakeSerdeDiff;
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
        $has_hint:expr,
        $hint_suffix:expr
    ) => {{
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    use $crate::traits::*;
                    let left_label = stringify!($left_label);
                    let right_label = stringify!($right_label);
                    let tup = (&*left_val, &*right_val);
                    let diff = tup.$method(left_label, right_label);
                    panic!(
                        "assertion failed: `({} == {})`{}{}'\
                           \n {:>label_padding$}: `{:?}`\
                           \n {:>label_padding$}: `{:?}`\
                           \n\n{}\n",
                        left_label,
                        right_label,
                        if $has_hint { ": " } else { "" },
                        $hint_suffix,
                        left_label,
                        diff._private_left(),
                        right_label,
                        diff._private_right(),
                        &diff,
                        label_padding = left_label.chars().count().max(right_label.chars().count())
                    );
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
        $crate::__assert_eq!(make_diff, $left_label, $left, $right_label, $right, false, "");
    });
    ($left_label:ident: $left:expr, $right_label:ident: $right:expr, $($arg:tt)*) => ({
        $crate::__assert_eq!(make_diff, $left_label, $left, $right_label, $right, true, format_args!($($arg)*));
    });
    ($left:expr, $right:expr $(,)?) => ({
        $crate::assert_eq!(left: $left, right: $right);
    });
    ($left:expr, $right:expr, $($arg:tt)*) => ({
        $crate::assert_eq!(left: $left, right: $right, $($arg)*);
    });
}

/// Asserts that two expressions are equal to each other (using [`PartialEq`]) using [`Serialize`](serde::Serialize) for comparision.
///
/// On panic, this macro will print the values of the expressions with their
/// serde [`Serialize`](serde::Serialize) representations rendered in the same
/// format that [`std::fmt::Debug`] would with a colorized diff of the changes in
/// the debug output.
///
/// Like [`assert!`], this macro has a second form, where a custom panic
/// message can be provided.
///
/// ```rust
/// use similar_asserts::assert_serde_eq;
/// assert_serde_eq!((1..3).collect::<Vec<_>>(), vec![1, 2]);
/// ```
///
/// This requires the `serde` feature.
#[macro_export]
#[cfg(feature = "serde")]
macro_rules! assert_serde_eq {
    ($left_label:ident: $left:expr, $right_label:ident: $right:expr $(,)?) => ({
        $crate::__assert_eq!(make_serde_diff, $left_label, $left, $right_label, $right, false, "");
    });
    ($left_label:ident: $left:expr, $right_label:ident: $right:expr, $($arg:tt)*) => ({
        $crate::__assert_eq!(make_serde_diff, $left_label, $left, $right_label, $right, true, format_args!($($arg)*));
    });
    ($left:expr, $right:expr $(,)?) => ({
        $crate::assert_serde_eq!(left: $left, right: $right);
    });
    ($left:expr, $right:expr, $($arg:tt)*) => ({
        $crate::assert_serde_eq!(left: $left, right: $right, $($arg)*);
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
