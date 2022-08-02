fn main() {
    let reference = (1..100).collect::<Vec<_>>();
    similar_asserts::assert_eq!(reference, (0..98).collect::<Vec<_>>());
}
