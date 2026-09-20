---
id: typechecking
title: Type checking
---

# Python type checking

The Python prelude can type check `python_library` and `python_binary` targets
(and their tests). Type checking is **opt-in**, and it needs a type checker to be
provided by your Python toolchain: the prelude does not ship one.

## Turning it on for a target

Set `typing = True` on the target:

```python
python_library(
    name = "lib",
    srcs = ["lib.py"],
    typing = True,
)
```

The related attributes are:

| Attribute                      | Meaning                                                                                                         |
| ------------------------------ | --------------------------------------------------------------------------------------------------------------- |
| `typing`                       | Whether to type check the target. Defaults to `False`.                                                          |
| `typing_validation`            | If `True` (and `typing` is `True`), type errors fail a normal `buck2 build` of the target. Defaults to `False`. |
| `py_version_for_type_checking` | Force the checker to check under a specific Python version.                                                     |
| `shard_typing`                 | Split the check into one action per source file, so files can be checked in parallel and cached independently.  |

## Running the type checker

When the toolchain provides a `type_checker`, each Python target gets a
`[typecheck]` subtarget. Build it to type check that target and its
dependencies:

```sh
buck2 build //path/to:lib[typecheck]
```

The output is a JSON file with the checker's results. If `typing` is not
enabled on the target, the subtarget builds successfully and produces an empty
result. If `shard_typing` is enabled, there is also one `[typecheck][shard_<path>]`
subtarget per source file.

To check many targets at once, use the BXL scripts in the prelude:

```sh
# Type check a set of targets or target patterns
buck2 bxl prelude//python/typecheck/batch.bxl:run -- --target //path/to/...

# Type check the targets that own the given files
buck2 bxl prelude//python/typecheck/batch_files.bxl:run -- --source path/to/file.py
```

Pass `--keep-going` to continue past targets that fail to load, and (for
`batch.bxl`) `--enable-sharding` to shard the check within each target.

## Providing a type checker

The type checker is set with the `type_checker` field of `PythonToolchainInfo`,
which takes a `RunInfo`. Optionally, `typeshed_stubs` (a `ManifestInfo`)
supplies the typeshed stubs that are passed to the checker.

```python
PythonToolchainInfo(
    # ...
    type_checker = ctx.attrs.type_checker[RunInfo],
    typeshed_stubs = ctx.attrs.typeshed_stubs[ManifestInfo],
)
```

You are responsible for the checker itself. Buck2 runs it as:

```
<type_checker> <config.json> --output <result.json>
```

### Input

`config.json` contains:

- `sources`: a list of manifests for the sources being checked
- `dependencies`: a list of manifests for the dependencies
- `typeshed`: the manifest of typeshed stubs
- `py_version`: the Python version to check against
- `system_platform`: the platform to check against

### Output

The checker must write a Pyre-compatible JSON object with an `errors` list to
`result.json`. Each error must include a `code`; `name` and `severity` are
optional. Errors that fail validation must also include `path`, `line`,
`column` and `description`.

Diagnostics with code `0`, with the name `unused-ignore` or `unused-type-ignore`,
or with a severity of `info`, `ignore` or `warn` do not fail validation.

The checker must exit successfully whenever it writes a valid result, **even if
that result contains type errors**. A nonzero exit code fails the type-checking
action itself, before Buck2 can turn the result into validation output.
