# Changelog

All notable changes to `similar-asserts` are documented here.

## 1.5.0

- Added automatic truncation of assertions.  This behavior can be overridden with the
  new `SIMILAR_ASSERTS_MAX_STRING_LENGTH` environment variable.  It defaults to `200`
  characters.