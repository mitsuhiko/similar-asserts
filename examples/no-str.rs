struct X;

impl PartialEq for X {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

fn main() {
    similar_asserts::assert_eq!(X, X);
}
