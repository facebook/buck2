# @generated
# To regenerate, run:
# ```
# STARLARK_RUST_REGENERATE_GOLDEN_TESTS=1 cargo test -p starlark --lib
# ```

error: Parse error: unexpected integer literal '1' here, expected one of "IDENTIFIER"
 --> assert.bzl:1:6
  |
1 | f'{x!1}'
  |      ^
  |
