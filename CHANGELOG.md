# Changelog

All notable changes to `similar-asserts` are documented here.

## Unreleased

## 2.0.0

- Raised the crate edition to Rust 2024 and updated the MSRV to Rust 1.85.
- Upgraded the `similar` dependency to `3.0.0`.
- Modernized CI workflows to current GitHub Actions and added PR runs.
- Replaced atomic env-var caching with `OnceLock`.
- Minor documentation and wording fixes.

## 1.7.0

- Added support for `SIMILAR_ASSERTS_CONTEXT_SIZE`. #13

## 1.6.1

- Maintenance release with some clippy fixes.

## 1.6.0

- Loosen static lifetime bounds for labels. #9

## 1.5.0

- Added automatic truncation of assertions.  This behavior can be overridden with the
  new `SIMILAR_ASSERTS_MAX_STRING_LENGTH` environment variable.  It defaults to `200`
  characters.
