# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog], and this project adheres to
[Semantic Versioning].

# Unreleased

None.

# 0.3.0 (16. Sep 2022)

- **updated:** To latest [rusqlite] version(`0.28`).

# 0.2.0 (13. July 2022)

- **changed:** Now using unbounded `crossbeam-channel` instead of bounded
  `std::sync::mpsc` channel internally.
- **changed:** Channel send errors in background database thread are now
  ignored instead of panicking.

# 0.1.0 (25. April 2022)

- Initial release.

[rusqlite]: https://crates.io/crates/rusqlite
[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/
[Semantic Versioning]: https://semver.org/spec/v2.0.0.html
