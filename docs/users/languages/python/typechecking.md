---
id: typechecking
title: Type checking
---

# Python type checking

The Python prelude can type check `python_library`, `python_binary` and
`python_test` targets. Type checking is **opt-in**, and it needs a type
checker to be provided by your Python toolchain: the prelude does not ship
one.

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
`[typecheck]` subtarget. Build it to type check that target's sources, with
its dependencies provided as inputs (the dependencies' own `[typecheck]`
subtargets are not built as part of this):

```sh
buck2 build //path/to:lib[typecheck]
```

The output is a JSON file with the checker's results. If `typing` is not
enabled on the target, the subtarget builds successfully and produces an
empty result.

`[typecheck]` also has its own sub-subtargets, which let you check (or
fetch the result of checking) a subset of the sources individually. If
`shard_typing` is **not** enabled (or typing is disabled), there is a single
`[typecheck][shard_default]` covering all sources. If `shard_typing` **is**
enabled, `shard_default` is replaced by one `[typecheck][shard_<path>]`
subtarget per source file instead, with `/` in the path sanitized to `+`
(for example, `[typecheck][shard_foo+bar.py]` for `foo/bar.py`).

To check many targets at once, use the BXL scripts in the prelude:

```sh
# Type check a set of targets or target patterns
buck2 bxl prelude//python/typecheck/batch.bxl:run -- --target //path/to/...

# Type check the targets that own the given files
buck2 bxl prelude//python/typecheck/batch_files.bxl:run -- --source path/to/file.py
```

Pass `--keep-going` to continue past targets that fail to load. (`batch.bxl`
also accepts an `--enable-sharding` flag, but as of this writing it has no
effect — sharding is controlled entirely by the per-target `shard_typing`
attribute.)

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
- `typeshed`: the manifest of typeshed stubs, or `null` if the toolchain
  doesn't set `typeshed_stubs`. Adapters that assume a manifest is always
  present will need to handle this case.
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

### What is a type checker, and where do typeshed stubs come from?

The `type_checker` is any executable that follows the [Input](#input) and
[Output](#output) contract described above; Buck2 does not ship one. Popular
choices include
[Pyre](https://pyre-check.org/), [mypy](https://mypy-lang.org/),
[Pyright](https://microsoft.github.io/pyright/) and
[ty](https://docs.astral.sh/ty/), Astral's type checker.

Typeshed stubs are `.pyi` files that describe the types of the standard
library and popular third-party packages, separately from their
implementation. A type checker needs them to check code that uses, for
example, `os` or `json`. Most checkers (including `ty`) bundle a copy of
[typeshed](https://github.com/python/typeshed) for the standard library, so
`typeshed_stubs` on the toolchain is only needed if you want to supply your
own (for example, pinned stubs for third-party packages, or a hermetic build
that must not rely on whatever the checker happened to bundle).

### Worked example: wiring up `ty`

No popular type checker speaks Buck2's exact contract out of the box: none of
them take a `config.json` in this shape, and most exit with a nonzero code
when they find type errors, which Buck2's contract forbids for a well-formed
result. In practice, `type_checker` points at a small adapter that translates
between the two. The following adapter has been tested against
[`ty`](https://docs.astral.sh/ty/) 0.0.84:

```python
#!/usr/bin/env python3
"""Adapter that lets Buck2's Python type checking use Astral's `ty`.

Invoked by Buck2 as: ty_adapter.py <config.json> --output <result.json>
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys


def read_manifest_paths(manifest_file: str) -> list[str]:
    """Return the real on-disk artifact paths listed in a manifest file."""
    with open(manifest_file, encoding="utf-8") as f:
        entries = json.load(f)
    # Each entry is [dest_path, artifact_path, origin].
    return [entry[1] for entry in entries]


def typeshed_root(manifest_file: str | None) -> str | None:
    """`ty` wants a single --typeshed directory, not a file list, so use the
    common parent of the typeshed manifest's stub files, if one was given.
    Falls back to ty's own vendored typeshed when no manifest is supplied.
    """
    if not manifest_file:
        return None
    paths = read_manifest_paths(manifest_file)
    if not paths:
        return None
    return os.path.commonpath([os.path.dirname(p) for p in paths])


SEVERITY_MAP = {
    "blocker": "error",
    "critical": "error",
    "major": "error",
    "minor": "warn",
    "info": "info",
}


def run_ty(source_files: list[str], py_version: str, typeshed: str | None) -> list[dict]:
    if not source_files:
        return []

    ty_bin = shutil.which("ty")
    if ty_bin is None:
        raise RuntimeError("`ty` was not found on PATH")

    # Buck2 actions don't necessarily run from a directory `ty` can walk up
    # from to find a project root (e.g. a sandboxed execution dir), so give
    # it one explicitly: the common parent of the files being checked.
    project = os.path.commonpath([os.path.dirname(p) for p in source_files])

    cmd = [
        ty_bin,
        "check",
        "--project",
        project,
        "--output-format",
        "gitlab",
        "--exit-zero",
        "--python-version",
        py_version,
    ]
    if typeshed:
        cmd += ["--typeshed", typeshed]
    cmd += source_files

    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        # --exit-zero should make this unreachable for ordinary type errors;
        # a nonzero code here means ty itself failed (bad args, crash, etc).
        raise RuntimeError(f"ty exited {proc.returncode} unexpectedly.\nstderr:\n{proc.stderr}")

    stdout = proc.stdout.strip()
    return json.loads(stdout) if stdout else []


def to_buck2_errors(gitlab_diagnostics: list[dict]) -> list[dict]:
    errors = []
    for diag in gitlab_diagnostics:
        location = diag.get("location", {})
        begin = location.get("positions", {}).get("begin", {})
        errors.append({
            "code": diag.get("check_name", "unknown"),
            "name": diag.get("check_name", "unknown"),
            "severity": SEVERITY_MAP.get(diag.get("severity"), "error"),
            "path": location.get("path", ""),
            "line": begin.get("line", 1),
            "column": begin.get("column", 1),
            "description": diag.get("description", ""),
        })
    return errors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("config")
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    result = {"errors": []}
    try:
        with open(args.config, encoding="utf-8") as f:
            config = json.load(f)

        source_files = []
        for manifest in config.get("sources") or []:
            source_files.extend(read_manifest_paths(manifest))

        py_version = config.get("py_version") or "3.12"
        typeshed = typeshed_root(config.get("typeshed"))

        diagnostics = run_ty(source_files, py_version, typeshed)
        result["errors"] = to_buck2_errors(diagnostics)
    except Exception as exc:
        # Surface adapter failures as a single type-checking error rather
        # than crashing: a nonzero exit here would fail the action before
        # Buck2 can even look at the JSON (per the contract above).
        result["errors"] = [{
            "code": "ty-adapter-error",
            "name": "ty-adapter-error",
            "severity": "error",
            "path": "",
            "line": 1,
            "column": 1,
            "description": f"{type(exc).__name__}: {exc}",
        }]

    with open(args.output, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)

    return 0  # Always succeed once result JSON has been written.


if __name__ == "__main__":
    sys.exit(main())
```

Wire it up as a `python_bootstrap_binary` (or any `RunInfo`-producing rule
that bundles `ty` or depends on it being on `PATH`) and point
`type_checker` at it:

```python
PythonToolchainInfo(
    # ...
    type_checker = ctx.attrs.ty_adapter[RunInfo],
    # `typeshed_stubs` left unset: `ty` falls back to its own vendored
    # typeshed for the standard library.
)
```

With this in place, `buck2 build //path/to:lib[typecheck]` runs `ty` under
the hood and reports its diagnostics through Buck2's normal validation
output.

This adapter is illustrative, not a maintained integration: it was tested
against a single-file target with `ty` 0.0.84, and the treatment of a custom
`typeshed_stubs` manifest (as opposed to `ty`'s bundled typeshed) is a
simplification that assumes the stub files share a common parent directory.
