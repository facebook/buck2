# @generated
# To regenerate, run:
# ```
# STARLARK_RUST_REGENERATE_DOC_TESTS=1 cargo test -p starlark --lib tests
# ```

# name

This is the summary of the module's docs

Some extra details can go here,
    and indentation is kept as expected

## f1

```python
def f1(
    a,
    b: str.type,
    c: int.type = 5,
    *,
    d: str.type = "some string",
    **kwargs
) -> [str.type]
```

Summary line goes here

#### Parameters

* `a`: The docs for a
* `b`: The docs for b
* `c`: The docs for c, but these go onto two lines
* `**kwargs`: Docs for the keyword args


#### Returns

A string repr of the args

---

## f2

```python
def f2(a, *args: [str.type])
```

This is a function with *args, and no return type

#### Parameters

* `*args`: Only doc this arg


---

## f3

```python
def f3(a: str.type) -> str.type
```

---

## f4

```python
def f4(a: str.type) -> str.type
```

This is a docstring with no 'Args:' section
