<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# build



### Members

| Member | Type | Description |
|--------|------|-------------|
| CommandExecutorConfig | `(bool, bool, *, Value < 'v >, Value < 'v >, NoneOr < i32 >, Value < 'v >, bool, bool, bool, bool) -> Value < 'v >` |  |
| cmd_args | `(*Vec < Value < 'v > >, Option < StringValue < 'v > >, Option < StringValue < 'v > >, Option < StringValue < 'v > >, Option < & str >) -> StarlarkCommandLine < 'v >` |  |
| get_base_path | `() -> String` |  |
| get_cell_name | `() -> String` |  |
| glob | `(Vec < String >, *, Option < Vec < String > >) -> Value < 'v >` |  |
| host_info | `() -> Value < 'v >` |  |
| implicit_package_symbol | `(& str, Option < Value < 'v > >) -> Value < 'v >` |  |
| load_symbols | `(DictRef < 'v >) -> NoneType` |  |
| package | `() -> String` |  |
| package_name | `() -> String` |  |
| provider | `(& str, Either < Vec < String >, SmallMap < & str, & str > >) -> ProviderCallable` |  |
| read_config | `(StringValue, StringValue, Option < Value < 'v > >) -> Value < 'v >` |  |
| regex_match | `(& str, & str) -> bool` |  |
| repository_name | `() -> String` |  |
| rule | `(Value < 'v >, DictOf < 'v, & 'v str, & 'v Attribute >, *, Option < Value >, & str, bool, bool) -> Value < 'v >` |  |
| rule_exists | `(& str) -> bool` | This should be called "target exists", not "rule exists" (if this should exist at all). |
| select | `(Value < 'v >) -> Selector < 'v >` |  |
| select_equal_internal | `(Value < 'v >, Value < 'v >) -> bool` | Tests that two selects are equal to each other. For testing use only. |
| select_map | `(Value < 'v >, Value < 'v >) -> Value < 'v >` | Applies a mapping function to a selector. See [Selector::select_map]. |
| select_test | `(Value < 'v >, Value < 'v >) -> bool` | Applies a test function to a selector. See [Selector::select_test]. |
| sha256 | `(& str) -> String` | Computes a sha256 digest for a string. Returns the hex representation of the digest. |
| transition | `(Value < 'v >, *, DictOf < 'v, StringValue < 'v >, StringValue < 'v > >, Option < Vec < StringValue < 'v > > >, bool) -> Transition < 'v >` |  |
| transitive_set | `(Option < SmallMap < String, Value < 'v > > >, Option < SmallMap < String, Value < 'v > > >) -> TransitiveSetDefinition < 'v >` |  |
| warning | `(& str) -> NoneType` | Produce a warning. |


## CommandExecutorConfig

```python
def CommandExecutorConfig(
    local_enabled: bool,
    remote_enabled: bool,
    *,
    remote_execution_properties: Value < 'v > = None,
    remote_execution_action_key: Value < 'v > = None,
    remote_execution_max_input_files_mebibytes: NoneOr < i32 > = None,
    remote_execution_use_case: Value < 'v > = None,
    use_limited_hybrid: bool = None,
    allow_limited_hybrid_fallbacks: bool = None,
    allow_hybrid_fallbacks_on_failure: bool = None,
    use_windows_path_separators: bool = None
) -> Value < 'v >
```

---
## cmd_args

```python
def cmd_args(*args: Vec < Value < 'v > >, delimiter: Option < StringValue < 'v > > = None, format: Option < StringValue < 'v > > = None, prepend: Option < StringValue < 'v > > = None, quote: Option < & str > = None) -> StarlarkCommandLine < 'v >
```

---
## get_base_path

```python
def get_base_path() -> String
```

---
## get_cell_name

```python
def get_cell_name() -> String
```

---
## glob

```python
def glob(include: Vec < String >, *, exclude: Option < Vec < String > > = None) -> Value < 'v >
```

---
## host_info

```python
def host_info() -> Value < 'v >
```

---
## implicit_package_symbol

```python
def implicit_package_symbol(name: & str, default: Option < Value < 'v > > = None) -> Value < 'v >
```

---
## load_symbols

```python
def load_symbols(symbols: DictRef < 'v >) -> NoneType
```

---
## package

```python
def package() -> String
```

---
## package_name

```python
def package_name() -> String
```

---
## provider

```python
def provider(doc: & str = None, fields: Either < Vec < String >, SmallMap < & str, & str > >) -> ProviderCallable
```

---
## read_config

```python
def read_config(section: StringValue, key: StringValue, default: Option < Value < 'v > > = None) -> Value < 'v >
```

---
## regex_match

```python
def regex_match(regex: & str, str: & str) -> bool
```

---
## repository_name

```python
def repository_name() -> String
```

---
## rule

```python
def rule(
    implementation: Value < 'v >,
    attrs: DictOf < 'v, & 'v str, & 'v Attribute >,
    *,
    cfg: Option < Value > = None,
    doc: & str = None,
    allow_unknown_attrs: bool = None,
    is_configuration_rule: bool = None
) -> Value < 'v >
```

---
## rule_exists

```python
def rule_exists(name: & str) -> bool
```

This should be called "target exists", not "rule exists" (if this should exist at all).

---
## select

```python
def select(d: Value < 'v >) -> Selector < 'v >
```

---
## select_equal_internal

```python
def select_equal_internal(left: Value < 'v >, right: Value < 'v >) -> bool
```

Tests that two selects are equal to each other. For testing use only.

---
## select_map

```python
def select_map(d: Value < 'v >, func: Value < 'v >) -> Value < 'v >
```

Applies a mapping function to a selector. See [Selector::select_map].

---
## select_test

```python
def select_test(d: Value < 'v >, func: Value < 'v >) -> bool
```

Applies a test function to a selector. See [Selector::select_test].

---
## sha256

```python
def sha256(val: & str) -> String
```

Computes a sha256 digest for a string. Returns the hex representation of the digest.

---
## transition

```python
def transition(implementation: Value < 'v >, *, refs: DictOf < 'v, StringValue < 'v >, StringValue < 'v > >, attrs: Option < Vec < StringValue < 'v > > > = None, split: bool = None) -> Transition < 'v >
```

---
## transitive_set

```python
def transitive_set(args_projections: Option < SmallMap < String, Value < 'v > > > = None, reductions: Option < SmallMap < String, Value < 'v > > > = None) -> TransitiveSetDefinition < 'v >
```

---
## warning

```python
def warning(x: & str) -> NoneType
```

Produce a warning.
