<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# build



### Members

| Member | Type | Description |
|--------|------|-------------|
| cmd_args | `(*Vec < Value < 'v > >, Option < String >, bool, Option < String >, Option < String >, Option < String >) -> StarlarkCommandLine < 'v >` |  |
| get_base_path | `() -> String` |  |
| get_cell_name | `() -> String` |  |
| glob | `(Vec < String >, Option < Vec < String > >, Option < Vec < String > >, bool) -> Value < 'v >` |  |
| host_info | `() -> Value < 'v >` |  |
| implicit_package_symbol | `(& str, Option < Value >) -> Value < 'v >` |  |
| load_symbols | `(DictRef) -> NoneType` |  |
| package | `() -> String` |  |
| package_name | `() -> String` |  |
| provider | `(& str, Either < Vec < String >, SmallMap < & str, & str > >) -> ProviderCallable` |  |
| read_config | `(StringValue, StringValue, Option < Value >) -> Value < 'v >` |  |
| regex_match | `(& str, & str) -> bool` |  |
| repository_name | `() -> String` |  |
| rule | `(Value, DictOf < & str, & Attribute >, Option < Value >, & str, bool, bool) -> Value < 'v >` |  |
| rule_exists | `(& str) -> bool` | This should be called "target exists", not "rule exists" (if this should exist at all). |
| select | `(Value) -> Selector < 'v >` |  |
| select_equal_internal | `(Value < 'v >, Value < 'v >) -> bool` | Tests that two selects are equal to each other. For testing use only. |
| select_map | `(Value < 'v >, Value < 'v >) -> Value < 'v >` | Applies a mapping function to a selector. See [Selector::select_map]. |
| select_test | `(Value < 'v >, Value < 'v >) -> bool` | Applies a test function to a selector. See [Selector::select_test]. |
| sha256 | `(& str) -> String` | Computes a sha256 digest for a string. Returns the hex representation of the digest. |
| transition | `(Value, DictOf < 'v, StringValue < 'v >, StringValue < 'v > >, bool) -> Transition < 'v >` |  |
| transitive_set | `(Option < SmallMap < String, Value < 'v > > >, Option < SmallMap < String, Value < 'v > > >) -> TransitiveSetDefinition < 'v >` |  |
| warning | `(& str) -> NoneType` | Produce a warning. |


## cmd_args

```python
def cmd_args(*args: Vec < Value < 'v > >, format: Option < String > = None, joined: bool = None, delimiter: Option < String > = None, quote: Option < String > = None, prepend: Option < String > = None) -> StarlarkCommandLine < 'v >
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
def glob(include: Vec < String >, excludes: Option < Vec < String > > = None, exclude: Option < Vec < String > > = None, include_dotfiles: bool = None) -> Value < 'v >
```

---
## host_info

```python
def host_info() -> Value < 'v >
```

---
## implicit_package_symbol

```python
def implicit_package_symbol(name: & str, default: Option < Value > = None) -> Value < 'v >
```

---
## load_symbols

```python
def load_symbols(symbols: DictRef) -> NoneType
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
def read_config(section: StringValue, key: StringValue, default: Option < Value > = None) -> Value < 'v >
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
def rule(implementation: Value, attrs: DictOf < & str, & Attribute >, cfg: Option < Value > = None, doc: & str = None, allow_unknown_attrs: bool = None, is_configuration_rule: bool = None) -> Value < 'v >
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
def select(d: Value) -> Selector < 'v >
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
def transition(implementation: Value, refs: DictOf < 'v, StringValue < 'v >, StringValue < 'v > >, split: bool = None) -> Transition < 'v >
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
