fn main() {
    let reference = "Hello\nWorld";
    similar_asserts::assert_eq!(reference, "Goodbye\nWorld");
}
