#[derive(Debug, PartialEq)]
enum Tag {
    Major,
    Minor,
    Value,
}

fn main() {
    let reference = vec![(Tag::Major, 2), (Tag::Minor, 20), (Tag::Value, 0)];

    similar_asserts::assert_eq!(
        expected: reference,
        actual:
            vec![
                (Tag::Major, 2),
                (Tag::Minor, 0),
                (Tag::Value, 0),
                (Tag::Value, 1)
            ],
        "some stuff here {}",
        42,
    );
}
