## Build buck2 with Cargo

From buck2 project root, run the following to build buck2 with cargo

```sh
cargo install --path=cli --root=/tmp
export BUCK2="/tmp/bin/buck2"
```

## Add in prelude into the project

For now, we can symlink in the project's prelude into the directory

```sh
ln -s $(realpath ../../prelude) prelude
```

If you are using any other prelude directory, make sure the prelude points to it in `.buckconfig`

## Sample commands

**_NOTE:_** These are currently only supported on Linux

```sh
$BUCK2 build //...
$BUCK2 run //python/hello_world:main
```
