## Build buck2 with Cargo

From buck2 project root, run the following to build buck2 with cargo

```sh
cargo install buck2 --root=/tmp
export BUCK2="/tmp/bin/buck2"
```

## Run `buck2 init --git`

Run `buck2 init` to initialize the prelude directory.

Now all targets aside from OCaml related ones are ready to be built.

## Support for building the example OCaml targets

The information in this section is (at this time) Linux and macOS specific.

The commands in `setup.sh` assume an [opam](https://opam.ocaml.org/) installation.

Some of these commands are intended to affect the current shell thus `setup.sh` is to be invoked via the `source` builtin e.g `source setup.sh`.

In addition to symlinking the prelude directory [as described above](#add-in-prelude-into-the-project), sourcing `setup.sh` activates the 'default' opam switch and creates symlinks in the 'third-party/opam' directory. These symlinks are necessary to support building the example OCaml targets. If any of the symlinks are found to already exist, they will not be overwritten.

## Sample commands

**_NOTE:_** These commands are currently only supported on Linux and macOS.

```sh
$BUCK2 build //...
$BUCK2 run //python/hello_world:main
```
