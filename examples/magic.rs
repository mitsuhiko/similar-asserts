use similar_asserts::assert;

#[derive(PartialEq, Debug)]
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    //assert!(Point { x: 1, y: 2 } == Point { x: 10, y: 2 });
    //assert!((1 + 2) == (3 + 4));
    assert!((1 + 2) >= (3 + 4));
}
