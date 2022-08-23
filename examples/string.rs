fn main() {
    let reference = "Hello\nWorld".to_string();
    similar_asserts::assert_eq!(reference, "Goodbye\nWorld");
}
