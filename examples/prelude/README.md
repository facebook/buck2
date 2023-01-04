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

On Linux and macOS, an alternative to symlinking the prelude manually in this way is to `source setup.sh` (see the next section for details).

## Support for building the example OCaml targets

The information in this section is (at this time) Linux and macOS specific.

The commands in `setup.sh` assume an [opam](https://opam.ocaml.org/) installation.

Some of these commands are intended to affect the current shell thus `setup.sh` is to be invoked via the `source` builtin e.g `source setup.sh`.

In addition to symlinking the prelude directory [as described above](#add-in-prelude-into-the-project), sourcing `setup.sh` activates the 'default' opam switch and creates symlinks in the 'third-party/opam' directory. These symlinks are neccessary to support building the example OCaml targets. If any of the symlinks are found to already exist, they will not be overwritten.

## Sample commands

**_NOTE:_** These commands are currently only supported on Linux and macOS.

```sh
$BUCK2 build //...
$BUCK2 run //python/hello_world:main
```
