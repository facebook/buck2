# @generated
# To regenerate, run:
# ```
# STARLARK_RUST_REGENERATE_DOC_TESTS=1 cargo test -p starlark --lib tests
# ```

# name

These are where the module docs go

## MAGIC

```python
MAGIC: int.type
```

---

## func1

```python
def func1(foo: str.type) -> str.type
```

Docs for func1

#### Parameters

* `foo`: Docs for foo


#### Returns

The string 'func1'

---

## func2

```python
def func2() -> str.type
```

---

## func3

```python
def func3(
    a1: int.type,
    a2: int.type = _,
    step: int.type = 1,
    /
) -> str.type
```

A function with only positional arguments.

#### `.type` attribute

Produces `"magic"`

#### Details

And a slightly longer description. With some example code:

```python
func3(1)
```

And some assertions:

```rust
1 == 1
```

---

## notypes

```python
def notypes(a: "") -> ""
```

---

## pos\_either\_named

```python
def pos_either_named(
    a: int.type,
    /,
    b: int.type,
    *,
    c: int.type
) -> None
```

---

## with\_defaults

```python
def with_defaults(
    explicit_default: [str.type] = [],
    hidden_default: [str.type] = _,
    string_default: str.type = "my_default"
) -> None
```
