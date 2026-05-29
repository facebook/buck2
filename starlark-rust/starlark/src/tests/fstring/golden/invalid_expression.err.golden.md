# @generated
# To regenerate, run:
# ```
# STARLARK_RUST_REGENERATE_GOLDEN_TESTS=1 cargo test -p starlark --lib
# ```

error: Parse error: unexpected identifier 'baz' here, expected one of "and", "not", "if", "or", "in", "==", "!=", "<=", ">=", "<", ">", "-", "+", "*", "%", "/", "//", ".", "&", "|", "^", "<<", ">>", "[", "(", "FSTRING_BANG" or "FSTRING_EXPR_END"
 --> assert.bzl:1:12
  |
1 | f'foo {bar baz}'
  |            ^^^
  |
