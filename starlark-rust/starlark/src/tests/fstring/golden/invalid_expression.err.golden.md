# @generated
# To regenerate, run:
# ```
# STARLARK_RUST_REGENERATE_GOLDEN_TESTS=1 cargo test -p starlark --lib
# ```

error: Parse error: unexpected identifier 'baz' here, expected one of "\n", "!=", "%", "%=", "&", "&=", "(", ")", "*", "*=", "+", "+=", ",", "-", "-=", ".", "/", "//", "//=", "/=", ":", ";", "<", "<<", "<<=", "<=", "=", "==", ">", ">=", ">>", ">>=", "FSTRING_BANG", "FSTRING_EXPR_END", "[", "]", "^", "^=", "and", "else", "for", "if", "in", "not", "or", "|", "|=" or "}"
 --> assert.bzl:1:12
  |
1 | f'foo {bar baz}'
  |            ^^^
  |
