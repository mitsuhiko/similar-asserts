all: test

build:
	@cargo build --all-features

check:
	@cargo check --all-features
	@cargo check --all-features --examples

doc:
	@cargo doc --all-features

test: check
	@cargo test
	@cargo test --all-features
	@cargo test --no-default-features

format:
	@rustup component add rustfmt 2> /dev/null
	@cargo fmt --all

format-check:
	@rustup component add rustfmt 2> /dev/null
	@cargo fmt --all -- --check

lint:
	@rustup component add clippy 2> /dev/null
	@cargo clippy

.PHONY: all doc test format format-check lint
