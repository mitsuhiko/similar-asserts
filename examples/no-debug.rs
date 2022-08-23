struct TypeWithoutDebug;

impl PartialEq for TypeWithoutDebug {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

fn main() {
    similar_asserts::assert_eq!(TypeWithoutDebug, TypeWithoutDebug);
}
