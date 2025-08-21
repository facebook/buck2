# @generated
# To regenerate, run:
# ```
# STARLARK_RUST_REGENERATE_GOLDEN_TESTS=1 cargo test -p starlark --lib
# ```

# globals

## MAGIC

```python
MAGIC: int
```

---

Docs for func1

### Function Signature

```python
def func1(foo: str) -> str
```

### Parameters

* `foo`: (required)

  Docs for foo



---



### Function Signature

```python
def func2() -> str
```

---



### Function Signature

```python
def pos_either_named(a: int, /, b: int, *, c: int) -> magic
```

---



### Function Signature

```python
def with_defaults(
    explicit_default: list[str] = [],
    hidden_default: list[str] = ...,
    string_default: str = "my_default",
) -> None
```
