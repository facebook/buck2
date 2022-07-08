# cell//foo/bar:baz.bzl

This is where we describe what the baz module is

Any extra details we want to add for the baz module
go here. These would be at the top of the file with
documented symbols in fbsource//foo/bar:baz.bzl

---
## CustomInfo

Summary of CustomInfo

Details about CustomInfo. These are
short, sweet, and mulitline

### Members

| Member | Type | Description |
|--------|------|-------------|
| bar | `UNKNOWN` | Summary of CustomInfo.bar |
| baz | `"string"` | Summary of CustomInfo.baz |
| foo | `"string"` | Summary of CustomInfo.foo |
| some_func | `("string", UNKNOWN, "string", UNKNOWN, *, *["string"], **UNKNOWN) -> "string"` | Summary of CustomInfo.some_func |


## bar : `UNKNOWN`

Summary of CustomInfo.bar

Details about CustomInfo.bar. These are
short, sweet, and mulitline

---
## baz : `"string"`

Summary of CustomInfo.baz

---
## foo : `"string"`

Summary of CustomInfo.foo

Details about CustomInfo.foo. These are
short, sweet, and mulitline

---
## some_func

```python
def some_func(
    a: "string",
    b: UNKNOWN,
    c: "string" = "c_default",
    d: UNKNOWN = "d_default",
    *,
    *args: ["string"],
    **kwargs: UNKNOWN
) -> "string"
```

Summary of CustomInfo.some_func

### Parameters

| Name | Details |
|------|---------|
| `a` | Docs for param `a` |
| `b` | Docs for param `b` |
| `c` | Docs for param `c` |
| `d` | Docs for param `d` |
| `*args` | Docs for param `*args` |
| `**kwargs` |  |

### Details

Details about CustomInfo.some_func. These are
short, sweet, and mulitline

### Returns

returns a value

---
## user_function_1

```python
def user_function_1(
    a: "string",
    b: UNKNOWN,
    c: "string" = "c_default",
    d: UNKNOWN = "d_default",
    *,
    *args: ["string"],
    **kwargs: UNKNOWN
) -> "string"
```

Summary of user_function_1

### Parameters

| Name | Details |
|------|---------|
| `a` | Docs for param `a` |
| `b` | Docs for param `b` |
| `c` | Docs for param `c` |
| `d` | Docs for param `d` |
| `*args` | Docs for param `*args` |
| `**kwargs` |  |

### Details

Details about user_function_1. These are
short, sweet, and mulitline

### Returns

returns a value

---
## user_function_2

```python
def user_function_2(
    a: "string",
    b: UNKNOWN,
    c: "string" = "c_default",
    d: UNKNOWN = "d_default",
    *,
    *args: ["string"],
    **kwargs: UNKNOWN
) -> "string"
```

Summary of user_function_2

### Parameters

| Name | Details |
|------|---------|
| `a` | Docs for param `a` |
| `b` | Docs for param `b` |
| `c` | Docs for param `c` |
| `d` | Docs for param `d` |
| `*args` | Docs for param `*args` |
| `**kwargs` |  |

### Details

Details about user_function_2. These are
short, sweet, and mulitline

### Returns

returns a value

---
## user_function_3

```python
def user_function_3(
    a: "string",
    b: UNKNOWN,
    c: "string" = "c_default",
    d: UNKNOWN = "d_default",
    *,
    *args: ["string"],
    **kwargs: UNKNOWN
) -> "string"
```

Summary of user_function_3

### Parameters

| Name | Details |
|------|---------|
| `a` | Docs for param `a` |
| `b` | Docs for param `b` |
| `c` | Docs for param `c` |
| `d` | Docs for param `d` |
| `*args` | Docs for param `*args` |
| `**kwargs` |  |

### Details

Details about user_function_3. These are
short, sweet, and mulitline

### Returns

returns a value
