# pydeer

This is a Python equivalent of Reindeer, the Cargo.lock -> BUCK file generator.

Usage:
```
$ buck2 run shim//third-party/python/pydeer:pydeer -- \
    buckify --third-party-dir shim/third-party/pypi --cell-root shim
```

This will read `uv.lock` in `shim/third-party/pypi` and create directories for each package with `BUCK.pydeer` files therein.

Each package directory has an eponymous `python_library` target e.g. `shim//third-party/pypi/pytest:pytest`, which consumers depend on.
It also has a `http_file` target which fetches a wheel for the package.

## Flags

`buckify`:

- `--third-party-dir <path>` - directory containing `pyproject.toml` and `uv.lock`; this is also the output directory.
- `--cell-root <path>` - path to the Buck2 cell root that contains `--third-party-dir`. Generated labels are relative to this. Defaults to `$BUCK2_OSS_REPO_DIR` or cwd. Pass this when the third-party dir lives in a non-root cell (e.g. `shim`).
- `--platforms linux-arm64,linux-x86_64,macos-arm64` - platforms to emit build rules for wheels for. By default, includes `linux-{arm64,x86_64}` and `macos-arm64`.
- `--python 3.11` - target Python version; determines which wheels are used.
- `--no-lock` - error out if uv.lock looks possibly outdated, rather than running `uv lock`
- `-v` - verbose logging

## Limitations

We do not support building packages from source yet: for now, we use the wheels for each platform and `select()` over them.

We don't emit dependency edges for extras (optional dependencies).
We ignore environment markers (e.g. `tomli ; python_version < "3.11"`) and emit the dependency edge unconditionally.
