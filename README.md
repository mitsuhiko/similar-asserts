# similar-asserts

[![Crates.io](https://img.shields.io/crates/d/similar-asserts.svg)](https://crates.io/crates/similar-asserts)
[![License](https://img.shields.io/github/license/mitsuhiko/similar-asserts)](https://github.com/mitsuhiko/similar-asserts/blob/main/LICENSE)
[![Documentation](https://docs.rs/similar-asserts/badge.svg)](https://docs.rs/similar-asserts)

`similar-asserts` is a crate that enhances the default assertion experience
by using [similar](https://crates.io/crates/similar) for diffing. It supports
comparing either `Debug` or `Serialize` representations of values. On failed
assertions it renders out a colorized diff to the terminal.

```rust
fn main() {
    let reference = vec![1, 2, 3, 4];
    similar_asserts::assert_eq!(reference, (0..4).collect::<Vec<_>>());
}
```

![](https://raw.githubusercontent.com/mitsuhiko/similar-asserts/main/assets/screenshot.png)

## Related Projects

* [insta](https://insta.rs) snapshot testing library
* [similar](https://insta.rs/similar) diffing library

## License and Links

- [Documentation](https://docs.rs/similar-asserts/)
- [Issue Tracker](https://github.com/mitsuhiko/similar-asserts/issues)
- [Examples](https://github.com/mitsuhiko/similar-asserts/tree/main/examples)
- License: [Apache-2.0](https://github.com/mitsuhiko/similar-asserts/blob/main/LICENSE)
