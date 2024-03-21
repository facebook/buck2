# @generated
# To regenerate, run:
# ```
# STARLARK_RUST_REGENERATE_GOLDEN_TESTS=1 cargo test -p starlark --lib tests
# ```

# name

These are where the module docs go

## MAGIC

```python
MAGIC: int
```

---

## func1

```python
def func1(foo: str) -> str
```

Docs for func1

#### Parameters

* `foo`: Docs for foo


#### Returns

The string 'func1'

---

## func2

```python
def func2() -> str
```

---

## func3

```python
def func3(
    a1: int,
    a2: int = _,
    step: int = 1,
    /
) -> str
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
def notypes(a)
```

---

## pos\_either\_named

```python
def pos_either_named(
    a: int,
    /,
    b: int,
    *,
    c: int
) -> None
```

---

## starlark\_args

```python
def starlark_args(*args: str) -> None
```

---

## starlark\_kwargs

```python
def starlark_kwargs(**kwargs: int) -> None
```

---

## with\_defaults

```python
def with_defaults(
    explicit_default: list[str] = [],
    hidden_default: list[str] = _,
    string_default: str = "my_default"
) -> None
```
