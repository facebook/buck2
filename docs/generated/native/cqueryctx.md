<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# cqueryctx



### Members

| Member | Type | Description |
|--------|------|-------------|
| allpaths | `("", "") -> "target_set"` | the `allpaths` query. |
| attrfilter | `(str.type, str.type, "") -> "target_set"` |  |
| attrregexfilter | `(str.type, str.type, "") -> "target_set"` |  |
| deps | `("", [None, int.type], [None, str.type]) -> "target_set"` |  |
| eval | `(str.type, [str.type], [None, [str.type]]) -> ""` | evaluates some general query string |
| filter | `(str.type, "") -> "target_set"` |  |
| inputs | `("") -> "file_set"` |  |
| kind | `(str.type, "") -> "target_set"` |  |
| owner | `([str.type, "file_set"]) -> "target_set"` |  |
| rdeps | `("", "", [None, int.type]) -> "target_set"` |  |
| somepaths | `("", "") -> "target_set"` |  |
| testsof | `("") -> "target_set"` |  |


## allpaths

```python
def allpaths(from: "", to: "") -> "target_set"
```

the `allpaths` query.

---
## attrfilter

```python
def attrfilter(attr: str.type, value: str.type, targets: "") -> "target_set"
```

---
## attrregexfilter

```python
def attrregexfilter(attribute: str.type, value: str.type, targets: "") -> "target_set"
```

---
## deps

```python
def deps(universe: "", depth: [None, int.type] = None, filter: [None, str.type] = None) -> "target_set"
```

---
## eval

```python
def eval(query: str.type, query_args: [str.type] = None, target_universe: [None, [str.type]] = None) -> ""
```

evaluates some general query string

---
## filter

```python
def filter(regex: str.type, targets: "") -> "target_set"
```

---
## inputs

```python
def inputs(targets: "") -> "file_set"
```

---
## kind

```python
def kind(regex: str.type, targets: "") -> "target_set"
```

---
## owner

```python
def owner(files: [str.type, "file_set"]) -> "target_set"
```

---
## rdeps

```python
def rdeps(universe: "", from: "", depth: [None, int.type] = None) -> "target_set"
```

---
## somepaths

```python
def somepaths(from: "", to: "") -> "target_set"
```

---
## testsof

```python
def testsof(targets: "") -> "target_set"
```
