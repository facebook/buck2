<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# cqueryctx



### Members

| Member | Type | Description |
|--------|------|-------------|
| allpaths | `(TargetExpr < ConfiguredTargetNode >, TargetExpr < ConfiguredTargetNode >) -> StarlarkTargetSet < ConfiguredTargetNode >` | the `allpaths` query. |
| attrfilter | `(& str, & str, TargetExpr < ConfiguredTargetNode >) -> StarlarkTargetSet < ConfiguredTargetNode >` |  |
| attrregexfilter | `(& str, & str, TargetExpr < ConfiguredTargetNode >) -> StarlarkTargetSet < ConfiguredTargetNode >` |  |
| deps | `(TargetExpr < ConfiguredTargetNode >, NoneOr < i32 >, NoneOr < & 'v str >) -> StarlarkTargetSet < ConfiguredTargetNode >` |  |
| kind | `(& str, TargetExpr < ConfiguredTargetNode >) -> StarlarkTargetSet < ConfiguredTargetNode >` |  |
| owner | `(FileSetExpr) -> StarlarkTargetSet < ConfiguredTargetNode >` |  |
| rdeps | `(TargetExpr < ConfiguredTargetNode >, TargetExpr < ConfiguredTargetNode >, Option < i32 >) -> StarlarkTargetSet < ConfiguredTargetNode >` |  |


## allpaths

```python
def allpaths(from: TargetExpr < ConfiguredTargetNode >, to: TargetExpr < ConfiguredTargetNode >) -> StarlarkTargetSet < ConfiguredTargetNode >
```

the `allpaths` query.

---
## attrfilter

```python
def attrfilter(attr: & str, value: & str, targets: TargetExpr < ConfiguredTargetNode >) -> StarlarkTargetSet < ConfiguredTargetNode >
```

---
## attrregexfilter

```python
def attrregexfilter(attribute: & str, value: & str, targets: TargetExpr < ConfiguredTargetNode >) -> StarlarkTargetSet < ConfiguredTargetNode >
```

---
## deps

```python
def deps(universe: TargetExpr < ConfiguredTargetNode >, depth: NoneOr < i32 > = None, filter: NoneOr < & 'v str > = None) -> StarlarkTargetSet < ConfiguredTargetNode >
```

---
## kind

```python
def kind(regex: & str, targets: TargetExpr < ConfiguredTargetNode >) -> StarlarkTargetSet < ConfiguredTargetNode >
```

---
## owner

```python
def owner(files: FileSetExpr) -> StarlarkTargetSet < ConfiguredTargetNode >
```

---
## rdeps

```python
def rdeps(universe: TargetExpr < ConfiguredTargetNode >, from: TargetExpr < ConfiguredTargetNode >, depth: Option < i32 > = None) -> StarlarkTargetSet < ConfiguredTargetNode >
```
