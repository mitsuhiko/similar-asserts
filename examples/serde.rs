use serde::Serialize;

#[derive(Serialize, PartialEq)]
pub enum MyEnum {
    One,
    Two,
}

#[derive(Serialize, PartialEq)]
pub struct Foo {
    a: Vec<u32>,
    b: MyEnum,
}

fn main() {
    let reference = Foo {
        a: vec![1, 2, 3, 4],
        b: MyEnum::One,
    };
    let actual = Foo {
        a: vec![1, 2, 4, 5],
        b: MyEnum::Two,
    };

    similar_asserts::assert_serde_eq!(&reference, &actual);
}
