<!--
@generated
Regenerate by running `buck2_docs --buck-command=buck2 --destination-dir=docs/generated --builtins fbcode//buck2/prelude:prelude.bzl`
-->

# build



### Members

| Member | Type | Description |
|--------|------|-------------|
| CommandExecutorConfig | `(bool.type, bool.type, *, "", "", [None, int.type], "", bool.type, bool.type, bool.type, bool.type) -> ""` |  |
| cmd_args | `(*[""], [None, str.type], [None, str.type], [None, str.type], [None, str.type]) -> "cmd_args"` |  |
| get_base_path | `() -> str.type` |  |
| get_cell_name | `() -> str.type` |  |
| glob | `([str.type], *, [None, [str.type]]) -> ""` |  |
| host_info | `() -> ""` |  |
| implicit_package_symbol | `(str.type, [None, ""]) -> ""` |  |
| load_symbols | `({"": ""}) -> None` |  |
| oncall | `(str.type) -> None` | Called in a TARGETS/BUCK file to declare the oncall contact details for all the targets defined. Must be called at most once, before any targets have been declared. Errors if called from a `.bzl` file. |
| package | `() -> str.type` |  |
| package_name | `() -> str.type` |  |
| provider | `(str.type, [[str.type], {str.type: str.type}]) -> "provider_callable"` |  |
| read_config | `(str.type, str.type, [None, ""]) -> ""` |  |
| regex_match | `(str.type, str.type) -> bool.type` |  |
| repository_name | `() -> str.type` |  |
| rule | `(*, [None, ""], [None, ""], {str.type: "attribute"}, [None, ""], str.type, bool.type, bool.type) -> ""` |  |
| rule_exists | `(str.type) -> bool.type` | This should be called "target exists", not "rule exists" (if this should exist at all). |
| select | `("") -> "selector"` |  |
| select_equal_internal | `("", "") -> bool.type` | Tests that two selects are equal to each other. For testing use only. |
| select_map | `("", "") -> ""` | Applies a mapping function to a selector. See [Selector::select_map]. |
| select_test | `("", "") -> bool.type` | Applies a test function to a selector. See [Selector::select_test]. |
| sha256 | `(str.type) -> str.type` | Computes a sha256 digest for a string. Returns the hex representation of the digest. |
| transition | `("", *, {str.type: str.type}, [None, [str.type]], bool.type) -> "transition"` |  |
| transitive_set | `([None, {str.type: ""}], [None, {str.type: ""}]) -> "transitive_set_definition"` |  |
| warning | `(str.type) -> None` | Produce a warning. |


## CommandExecutorConfig

```python
def CommandExecutorConfig(
    local_enabled: bool.type,
    remote_enabled: bool.type,
    *,
    remote_execution_properties: "" = None,
    remote_execution_action_key: "" = None,
    remote_execution_max_input_files_mebibytes: [None, int.type] = None,
    remote_execution_use_case: "" = None,
    use_limited_hybrid: bool.type = None,
    allow_limited_hybrid_fallbacks: bool.type = None,
    allow_hybrid_fallbacks_on_failure: bool.type = None,
    use_windows_path_separators: bool.type = None
) -> ""
```

---
## cmd_args

```python
def cmd_args(*args: [""], delimiter: [None, str.type] = None, format: [None, str.type] = None, prepend: [None, str.type] = None, quote: [None, str.type] = None) -> "cmd_args"
```

---
## get_base_path

```python
def get_base_path() -> str.type
```

---
## get_cell_name

```python
def get_cell_name() -> str.type
```

---
## glob

```python
def glob(include: [str.type], *, exclude: [None, [str.type]] = None) -> ""
```

---
## host_info

```python
def host_info() -> ""
```

---
## implicit_package_symbol

```python
def implicit_package_symbol(name: str.type, default: [None, ""] = None) -> ""
```

---
## load_symbols

```python
def load_symbols(symbols: {"": ""}) -> None
```

---
## oncall

```python
def oncall(_name: str.type) -> None
```

Called in a TARGETS/BUCK file to declare the oncall contact details for all the targets defined. Must be called at most once, before any targets have been declared. Errors if called from a `.bzl` file.

---
## package

```python
def package() -> str.type
```

---
## package_name

```python
def package_name() -> str.type
```

---
## provider

```python
def provider(doc: str.type = None, fields: [[str.type], {str.type: str.type}]) -> "provider_callable"
```

---
## read_config

```python
def read_config(section: str.type, key: str.type, default: [None, ""] = None) -> ""
```

---
## regex_match

```python
def regex_match(regex: str.type, str: str.type) -> bool.type
```

---
## repository_name

```python
def repository_name() -> str.type
```

---
## rule

```python
def rule(
    *,
    implementation: [None, ""] = None,
    impl: [None, ""] = None,
    attrs: {str.type: "attribute"},
    cfg: [None, ""] = None,
    doc: str.type = None,
    is_configuration_rule: bool.type = None,
    is_toolchain_rule: bool.type = None
) -> ""
```

---
## rule_exists

```python
def rule_exists(name: str.type) -> bool.type
```

This should be called "target exists", not "rule exists" (if this should exist at all).

---
## select

```python
def select(d: "") -> "selector"
```

---
## select_equal_internal

```python
def select_equal_internal(left: "", right: "") -> bool.type
```

Tests that two selects are equal to each other. For testing use only.

---
## select_map

```python
def select_map(d: "", func: "") -> ""
```

Applies a mapping function to a selector. See [Selector::select_map].

---
## select_test

```python
def select_test(d: "", func: "") -> bool.type
```

Applies a test function to a selector. See [Selector::select_test].

---
## sha256

```python
def sha256(val: str.type) -> str.type
```

Computes a sha256 digest for a string. Returns the hex representation of the digest.

---
## transition

```python
def transition(implementation: "", *, refs: {str.type: str.type}, attrs: [None, [str.type]] = None, split: bool.type = None) -> "transition"
```

---
## transitive_set

```python
def transitive_set(args_projections: [None, {str.type: ""}] = None, reductions: [None, {str.type: ""}] = None) -> "transitive_set_definition"
```

---
## warning

```python
def warning(x: str.type) -> None
```

Produce a warning.
