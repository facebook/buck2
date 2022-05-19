<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# ctx.actions



### Members

| Member | Type | Description |
|--------|------|-------------|
| args | `(Option < Value < 'v > >, Option < String >, bool, Option < String >, Option < String >, Option < String >) -> StarlarkCommandLine < 'v >` |  |
| copy | `(Value < 'v >, Value < 'v >) -> Value < 'v >` |  |
| declare_output | `(& str, Option < & str >) -> StarlarkDeclaredArtifact` |  |
| download_file | `(String, Value < 'v >, Option < String >, Option < String >, bool) -> Value < 'v >` |  |
| dynamic_output | `(Vec < StarlarkArtifact >, Vec < StarlarkArtifact >, Vec < StarlarkOutputArtifact >, Value) -> NoneType` |  |
| run | `(Value < 'v >, String, NoneOr < String >, Option < ValueOf < SmallMap < & str, Value < 'v > > > >, bool, bool, i32) -> NoneType` |  |
| symlinked_dir | `(Value < 'v >, Value < 'v >, bool) -> Value < 'v >` |  |
| tset | `(Value < 'v >, Option < Value < 'v > >, Option < Value < 'v > >) -> Value < 'v >` |  |
| write | `(Value < 'v >, Value < 'v >, bool, bool) -> Value < 'v >` |  |


## args

```python
def args(values: Option < Value < 'v > > = None, format: Option < String > = None, joined: bool = None, delimiter: Option < String > = None, quote: Option < String > = None, prepend: Option < String > = None) -> StarlarkCommandLine < 'v >
```

---
## copy

```python
def copy(src: Value < 'v >, dest: Value < 'v >) -> Value < 'v >
```

---
## declare_output

```python
def declare_output(prefix: & str, filename: Option < & str > = None) -> StarlarkDeclaredArtifact
```

---
## download_file

```python
def download_file(url: String, output: Value < 'v >, sha1: Option < String > = None, sha256: Option < String > = None, is_executable: bool = None) -> Value < 'v >
```

---
## dynamic_output

```python
def dynamic_output(dynamic: Vec < StarlarkArtifact >, inputs: Vec < StarlarkArtifact >, outputs: Vec < StarlarkOutputArtifact >, lambda: Value) -> NoneType
```

---
## run

```python
def run(
    arguments: Value < 'v >,
    category: String,
    identifier: NoneOr < String > = None,
    env: Option < ValueOf < SmallMap < & str, Value < 'v > > > > = None,
    local_only: bool = None,
    always_print_stderr: bool = None,
    weight: i32 = None
) -> NoneType
```

---
## symlinked_dir

```python
def symlinked_dir(output: Value < 'v >, srcs: Value < 'v >, copy: bool = None) -> Value < 'v >
```

---
## tset

```python
def tset(definition: Value < 'v >, value: Option < Value < 'v > > = None, children: Option < Value < 'v > > = None) -> Value < 'v >
```

---
## write

```python
def write(output: Value < 'v >, content: Value < 'v >, is_executable: bool = None, allow_args: bool = None) -> Value < 'v >
```
