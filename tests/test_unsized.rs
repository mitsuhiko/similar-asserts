#[test]
fn test_unsized() {
    similar_asserts::assert_eq!("foo".to_string(), "bfoo"[1..]);
}
