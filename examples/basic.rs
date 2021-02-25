fn main() {
    let reference = vec![1, 2, 3, 4];
    similar_asserts::assert_eq!(reference, (0..4).collect::<Vec<_>>());
}
