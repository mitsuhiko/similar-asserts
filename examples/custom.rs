use similar_asserts::SimpleDiff;

fn main() {
    panic!(
        "Not equal\n\n{}",
        SimpleDiff::from_str("a\nb\n", "b\nb\n", "left", "right")
    );
}
