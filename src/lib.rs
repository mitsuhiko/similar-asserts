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
//! - [`assert_str_eq!`]: compares both sides to strings and diffs those on failure.
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
#[doc(hidden)]
pub struct Diff<'a> {
    left: Cow<'a, str>,
    right: Cow<'a, str>,
    left_short: Option<Cow<'a, str>>,
    right_short: Option<Cow<'a, str>>,
    left_label: &'static str,
    right_label: &'static str,
}

impl<'a> Diff<'a> {
    /// Creates a diff from two values implementing [`Debug`].
    pub fn from_debug<Left: fmt::Debug, Right: fmt::Debug>(
        left: &Left,
        right: &Right,
        left_label: &'static str,
        right_label: &'static str,
    ) -> Diff<'a> {
        let left_short = Some(format!("{:?}", left).into());
        let right_short = Some(format!("{:?}", right).into());
        let left = format!("{:#?}", left).into();
        let right = format!("{:#?}", right).into();
        Diff {
            left,
            right,
            left_short,
            right_short,
            left_label,
            right_label,
        }
    }

    /// Creates a diff from two values implementing [`Debug`].
    #[cfg(feature = "serde")]
    pub fn from_serde<Left: serde::Serialize, Right: serde::Serialize>(
        left: &Left,
        right: &Right,
        left_label: &'static str,
        right_label: &'static str,
    ) -> Diff<'a> {
        let left_short = Some(format!("{:?}", serde_impl::Debug(left)).into());
        let right_short = Some(format!("{:?}", serde_impl::Debug(right)).into());
        let left = format!("{:#?}", serde_impl::Debug(left)).into();
        let right = format!("{:#?}", serde_impl::Debug(right)).into();
        Diff {
            left,
            right,
            left_short,
            right_short,
            left_label,
            right_label,
        }
    }

    /// Creates a diff from two strings.
    pub fn from_str(
        left: &'a str,
        right: &'a str,
        left_label: &'static str,
        right_label: &'static str,
    ) -> Diff<'a> {
        Diff {
            left: left.into(),
            right: right.into(),
            left_short: None,
            right_short: None,
            left_label,
            right_label,
        }
    }

    pub fn left(&self) -> &str {
        self.left_short.as_deref().unwrap_or(&self.left)
    }

    pub fn right(&self) -> &str {
        self.right_short.as_deref().unwrap_or(&self.right)
    }
}

impl<'a> fmt::Display for Diff<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.left == self.right {
            writeln!(
                f,
                "{}: {}",
                style("Invisible differences").bold(),
                "the two values are the same in string form."
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
        for (idx, group) in diff.grouped_ops(5).into_iter().enumerate() {
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
        $has_hint:expr,
        $hint_suffix:expr
    ) => {{
        match (&($left), &($right)) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    let left_label = stringify!($left_label);
                    let right_label = stringify!($right_label);
                    let diff =
                        $crate::Diff::$method(&*left_val, &*right_val, left_label, right_label);
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
                        diff.left(),
                        right_label,
                        diff.right(),
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
/// debug representations with a colorized diff of the changes in the debug
/// output.
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
        $crate::__assert_eq!(from_debug, $left_label, $left, $right_label, $right, false, "");
    });
    ($left_label:ident: $left:expr, $right_label:ident: $right:expr, $($arg:tt)*) => ({
        $crate::__assert_eq!(from_debug, $left_label, $left, $right_label, $right, true, format_args!($($arg)*));
    });
    ($left:expr, $right:expr $(,)?) => ({
        $crate::assert_eq!(left: $left, right: $right);
    });
    ($left:expr, $right:expr, $($arg:tt)*) => ({
        $crate::assert_eq!(left: $left, right: $right, $($arg)*);
    });
}

/// Asserts that two expressions are equal to each other (using [`PartialEq`]).
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
        $crate::__assert_eq!(from_serde, $left_label, $left, $right_label, $right, false, "");
    });
    ($left_label:ident: $left:expr, $right_label:ident: $right:expr, $($arg:tt)*) => ({
        $crate::__assert_eq!(from_serde, $left_label, $left, $right_label, $right, true, format_args!($($arg)*));
    });
    ($left:expr, $right:expr $(,)?) => ({
        $crate::assert_serde_eq!(left: $left, right: $right);
    });
    ($left:expr, $right:expr, $($arg:tt)*) => ({
        $crate::assert_serde_eq!(left: $left, right: $right, $($arg)*);
    });
}

/// Asserts that two stringified expressions are equal to each other
/// (using [`PartialEq`]).
///
/// This works similar to [`assert_eq!`] but before comparing the values they
/// are stringified via [`ToString`].  This also uses a slightly different
/// comparison display that is better optimized for normal strings.
///
/// On panic, this macro will print the values of the expressions with their
/// debug representations with a colorized diff of the changes in the debug
/// output.
///
/// Like [`assert!`], this macro has a second form, where a custom panic
/// message can be provided.
///
/// ```rust
/// use similar_asserts::assert_str_eq;
/// assert_str_eq!("foobarbaz".replace("z", "zzz"), "foobarbazzz");
/// ```
#[macro_export]
macro_rules! assert_str_eq {
    ($left_label:ident: $left:expr, $right_label:ident: $right:expr $(,)?) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                let left_val = left_val.to_string();
                let right_val = right_val.to_string();
                $crate::__assert_eq!(from_str, $left_label, left_val, $right_label, right_val, false, "");
            }
        }
    });
    ($left_label:ident: $left:expr, $right_label:ident: $right:expr, $($arg:tt)*) => ({
        match (&($left), &($right)) {
            (left_val, right_val) => {
                let left_val = left_val.to_string();
                let right_val = right_val.to_string();
                $crate::__assert_eq!(from_str, $left_label, left_val, $right_label, right_val, true, format_args!($($arg)*));
            }
        }
    });
    ($left:expr, $right:expr $(,)?) => ({
        $crate::assert_str_eq!(left: $left, right: $right);
    });
    ($left:expr, $right:expr, $($arg:tt)*) => ({
        $crate::assert_str_eq!(left: $left, right: $right, $($arg)*);
    });
}
