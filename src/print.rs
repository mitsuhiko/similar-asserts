use std::borrow::Cow;
use std::fmt::Debug;

pub trait StringRepr: AsRef<str> {}

impl StringRepr for str {}
impl StringRepr for String {}
impl StringRepr for Cow<'_, str> {}
impl<T: StringRepr + ?Sized> StringRepr for &T {}

/// Defines how the object is printed.
pub enum PrintMode {
    /// The regular print mode.  If an object does not return
    /// something for this print mode it's not formattable.
    Default,
    /// Some objects have an extra expanded print mode with pretty newlines.
    Expanded,
}

pub trait PrintObject<'a> {
    fn print_object(self, mode: PrintMode) -> Option<Cow<'a, str>>;
}

impl<'a, 'b: 'a, T: StringRepr + ?Sized + 'a> PrintObject<'a> for (&'b T,) {
    fn print_object(self, mode: PrintMode) -> Option<Cow<'a, str>> {
        match mode {
            PrintMode::Default => Some(Cow::Borrowed(self.0.as_ref())),
            PrintMode::Expanded => None,
        }
    }
}

impl<'a, 'b: 'a, T: Debug + 'a> PrintObject<'a> for &'b (T,) {
    fn print_object(self, mode: PrintMode) -> Option<Cow<'a, str>> {
        Some(
            match mode {
                PrintMode::Default => format!("{:?}", self.0),
                PrintMode::Expanded => format!("{:#?}", self.0),
            }
            .into(),
        )
    }
}

impl<'a, 'b: 'a, T: 'a> PrintObject<'a> for &'b mut (T,) {
    fn print_object(self, _mode: PrintMode) -> Option<Cow<'a, str>> {
        fn type_name_of_val<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let s = type_name_of_val(&self.0).trim_start_matches('&');
        if s.is_empty() {
            None
        } else {
            Some(Cow::Borrowed(s))
        }
    }
}

#[test]
fn test_object() {
    macro_rules! print_object {
        ($expr:expr, $mode:ident) => {{
            use $crate::print::PrintObject;
            #[allow(unused_mut)]
            let mut _tmp = ($expr,);
            _tmp.print_object($crate::print::PrintMode::$mode)
                .map(|x| x.to_string())
        }};
    }

    struct NoDebugNoString;

    struct DoNotCallMe;

    impl DoNotCallMe {
        #[allow(unused)]
        fn print_object(&self, mode: PrintMode) {
            panic!("never call me");
        }
    }

    assert_eq!(
        print_object!(&DoNotCallMe, Default).as_deref(),
        Some("similar_asserts::print::test_object::DoNotCallMe")
    );
    assert_eq!(
        print_object!(&NoDebugNoString, Default).as_deref(),
        Some("similar_asserts::print::test_object::NoDebugNoString")
    );
    assert_eq!(
        print_object!(vec![1, 2, 3], Default).as_deref(),
        Some("[1, 2, 3]")
    );
    assert_eq!(
        print_object!(vec![1, 2, 3], Expanded).as_deref(),
        Some("[\n    1,\n    2,\n    3,\n]")
    );
    assert_eq!(print_object!(&"Hello", Default).as_deref(), Some("Hello"));
    assert_eq!(print_object!(&"Hello", Expanded).as_deref(), None);
}
