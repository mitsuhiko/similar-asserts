fn main() {
    let reference = "foo\r\nbar";
    similar_asserts::assert_eq!(reference, "foo\nbar");
}
