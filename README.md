# Buck2

[Documentation](https://www.internalfb.com/intern/staticdocs/buck2/); [User group](https://fb.workplace.com/groups/buck2users); [Announcement group](https://fb.workplace.com/groups/2186885101608978/);
[oncall](https://www.internalfb.com/intern/monitor/oncall_profile?oncall_id=303358434096579&oncall=buck2).

This directory contains the code for the Buck2 build system - the successor to the original Buck build system.

## Development

For the setup guide and basics, continue reading below. For tips and techniques for debugging, see [`docs/developers/developers.md`](docs/developers/developers.md).

Commands below assume the use of bash/zsh, modify accordingly for other shells.

### Eden users

If you use EdenFS backed repository (the default settings) we highly recommend setting `$CARGO_TARGET_DIR` to `$HOME/cargo-target-buck2` to make Cargo put its files elsewhere.

```shell
export CARGO_TARGET_DIR=$HOME/cargo-target-buck2
```

This dramatically reduces compilation times and improves cargo stability.

### macOS setup

To set up a macOS machine run:

```shell
fbclone fbcode --eden [dest_dir] # dest_dir is optional
```

Next, install the Rust toolchain using:

```shell
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

CMake has to be installed to compile protobuf:

NB: M1 machines need a special homebrew setup, see [the post](https://fb.workplace.com/groups/hack.of.the.day/permalink/1925865110834868/).

```
brew install cmake
```

Finally, you need to include Rust's toolchain in your `PATH`. Depending on your shell, you should put the following directive in either `~/.profile`, `~/.bashrc`, `~/.zshrc` or `~/.zshenv`. Consult your shell documentation.

NB: macOS Catalina (10.15) changed the default shell from Bash to Zsh.

NB: The Rust toolchain probably already adjusted your shell environment (it says as part of the installation). If that's the case, you don't need to add the following directive.

```shell
source "$HOME/.cargo/env"
```

### Devserver setup

To set up a Linux devserver run:

```shell
fbclone fbcode --eden
sudo feature install ttls_fwdproxy
systemctl --user start ttls_fwdproxy_loader.service
sudo dnf install elfutils-libelf-devel cmake

mkdir -p "$HOME/.cargo"
echo '[http]' >> "$HOME/.cargo/config"
echo 'proxy="fwdproxy:8080"' >> "$HOME/.cargo/config"
(
  export HTTPS_PROXY=fwdproxy:8080
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain none
)
. "$HOME/.cargo/env"
```

If cargo complains about SSL peer certificate or SSH remote key error, then you can try adding `proxy = http://fwdproxy:8080` under http in `~/.gitconfig`.

### Windows setup

To set up a Windows machine run:

```shell
fbclone fbsource --eden [dest_dir] # dest_dir is optional
```

Install OpenSSL to have no warnings from rust-analyzer:

```shell
cd C:\open
git clone https://github.com/microsoft/vcpkg
cd vcpkg
.\bootstrap-vcpkg.bat
.\vcpkg.exe integrate install
.\vcpkg.exe install openssl:x64-windows-static-md
```

Then in Administrator terminal:

```shell
choco install cmake -s 'https://chocolatey.org/api/v2/'
```

Install Rust toolchain: https://win.rustup.rs/x86_64

### Incremental development

To develop incrementally you should copy setup Rust on your path using `. "$HOME/.cargo/env"` in a shell startup file (e.g. `~/.profile`). You can then use `cargo build` or a Rust Analyser IDE. There are a few useful scripts:

* `./test.py` which checks the build for formatting, warnings, errors, lint violations and runs the unit tests. Running `./test.py <project>` (where `<project>` is something like `gazebo`, `starlark`, `buck2_build_api` etc - see the root `Cargo.toml`) will only test one specific project. Adding `--lint-only` will only do linting (not compile or test), and `--lint-rust-only` will not lint the Starlark code.
* `./test_e2e.sh` which runs end to end tests building test targets. Run `./test_e2e.sh <testname>`  (where `<testname>` is something like `test_cached_build`, `test_symlink_dir` etc - see the `tests/e2e/*.py` files) to only run one test.
* `./buck2.sh` which compiles and runs a local `buck2` binary.

These scripts are run in CI (by `scripts/ci.sh`). Do not merge your code unless the `buck2` CI job is successful.

### VS Code setup

We recommend using [Rust Analyzer project](https://github.com/rust-analyzer/rust-analyzer) with either [VS Code @ FB - Insiders](https://www.internalfb.com/intern/wiki/Vscode/#insiders-program) or normal [VS Code](https://code.visualstudio.com/), running locally (not on a devserver), opening `buck2` as the root folder of the workspace (File, Open). We recommend the following settings:

```json
"[rust]": {
    "editor.formatOnSave": true,
},
"rust-analyzer.diagnostics.disabled": ["unresolved-import"],
"rust-analyzer.procMacro.enable": true,
"rust-analyzer.assist.importPrefix": "crate",
"files.trimTrailingWhitespace": true,
"files.trimFinalNewlines": true,
```

To add the settings, show the Command Palette (Cmd-Shift-P on macOS), type "settings" and select "Preferences: Open Settings (JSON)". For more info, see [here](https://code.visualstudio.com/docs/getstarted/settings).

When using VSCode @ FB Insiders, you can open fbsource instead of buck2 as the root folder of the workspace. To do so, you need to add this line to the settings json:

```json
"rust-analyzer.linkedProjects": [
    "fbcode/buck2/Cargo.toml"
],
```

Note that Rust Analyzer is not supported by VS Code @ FB when using remote development ([see](https://fb.workplace.com/groups/2186885101608978/permalink/2595114034119414/)).

To debug Rust code install [the CodeLLDB extension](https://marketplace.visualstudio.com/items?itemName=vadimcn.vscode-lldb). This extension is not supported by VS Code @ FB _when_ using remote development ([see](https://fb.workplace.com/groups/2186885101608978/permalink/2595114034119414/)). It _does_ work when using VS Code @ FB _locally_. Be aware that starting a debugger can easily take 10-20 seconds, and may not show any feedback while it is starting, so be patient and don't start the debugger repeatedly.

* To debug a test, set whatever breakpoints you want, then click the `Debug` hover link above the `#[test]` annotation.
* To debug a binary, hit the `Run` icon (play icon with an insect beneath it) on the VS Code left menu and you should see a little green play icon at the top with `Debug buck-build`. Set whatever breakpoints you want, then hit the green play icon. To modify the command or arguments (it defaults to building `process_wrapper` with `buck-build`) see `.vscode/launch.json` in the `buck2` directory (or just click the cog icon next to "Debug buck-build"). In most cases, don't commit your modifications. See this [screenshot](https://pxl.cl/1Cp5F) for visual guidance.
* To debug a running buck2 daemon, see the instructions in [`docs/developers/developers.md`](docs/developers/developers.md).

### Starlark VS Code Extension

Our team maintains a generic Starlark VS Code extension that gives IDE support for build files and .bzl files. It is under active development and will be gaining features regularly.

There are two copies of the VS Code Extension. One is for internal use, and is at `fbsource/vscode/vscode-extensions/starlark`, and the other, for OSS use, is at `starlark-rust/vscode`.

The key difference is that the internal version integrates with our internal VSCode infrastructure, and has some extra feature gating, interaction with the old buck extension, and uses our metrics framework. It also has some different setting defaults, like using `buck2` as the LSP binary instead of `starlark`

This plugin works by talking to `buck2 lsp` over stdin/stderr. As features are implemented in buck2's LSP server, they will automatically start working in the plugin.

If trying to build and install the OSS plugin on an eden FS, it can help to add a redirect to the vscode plugin's node_modules directory:

```shell
eden redirect add fbcode/buck2/starlark-rust/vscode/node_modules bind
```

To use the internal plugin for development, see the README in `fbsource/vscode/vscode-extensions/starlark`.

To use the OSS plugin for development, follow the instructions at `starlark-rust/vscode/README.md`, or the abbreviated ones here.

- `cd starlark-rust/vscode && npm install && npm install vsce && npm exec vsce package`
- In VS Code, go to Extensions, click on the "..." button in the Extensions bar, select "Install from VSIX" and then select the `starlark-1.0.0.vsix` file at `$FBCODE/buck2/starlark-rust/vscode/starlark-1.0.0.vsix`
- Update the extension settings to point to `buck2` for `starlark.lspPath`, and `["lsp"]` for `starlark.lspArguments`. This can be done in the VS Code UI by going to the extension's settings panel.

If you want to use a local build of buck2, just change the path to `$FBCODE/buck2/scripts/buck2_lsp.sh` and unset `starlark.lspArguments`.

NOTE: If you're seeing slightly confusing behavior, make sure that you're using "Starlark" as the language for files that you're editing.

### Upgrading dependencies

To upgrade our dependencies:

* `cargo update` will rewrite the `Cargo.lock` file. This file is not checked in, so the impact will be to make your builds more like CI (which generated the file afresh each time).
* Change the `rust-toolchain` file to use a newer nightly compiler. You can see [what components are in which version](https://rust-lang.github.io/rustup-components-history/).
* [`cargo outdated`](https://github.com/kbknapp/cargo-outdated) will tell you what bounds could be relaxed.
* Validate with `./test.py`.

## Troubleshooting

* If Remote Execution throws errors, make sure you are on the VPN/Lighthouse, then try `kinit`, `cc-certs` and `fixmymac`.
* If you get linker errors for missing `.so` files, try running `sudo dnf install elfutils-libelf-devel libseccomp-devel -y`. You can also find the missing package using `sudo dnf search` command.
* If you get compile errors in generated Thrift code when building with Cargo, try [updating the Thrift generator](https://www.internalfb.com/intern/msdk/bump/?action=view&schedule_fbid=982424652505230).
* If you get the output "error: could not download file from..." when building for the first time with Cargo on a devserver, make sure you've added the `ttls_proxy` feature and try addding `HTTPS_PROXY=fwdproxy:8080` right before the build command. You'll need to do this every time the toolchain is updated.

## Terminology conventions

* A _target_, e.g. `fbcode//eden:eden`, is something a user defines that is an instance of a _rule_, which can be built.
* A _rule_, e.g. `cxx_library`, is an implementation of how something is built.
* _Loading_ a `TARGETS`/`BUCK` file involves evaluating the Starlark and doing attribute coercion/resolution. It can be done with `buck2 cquery fbcode//eden:eden` or `buck2 cquery 'deps(fbcode//eden:eden)'` to do it recursively.
* _Analysing_ a _target_ involves running the associated _rule_ to produce the _providers_. It can be done with `buck2 audit providers fbcode//eden:eden`.
* _Building_ a _target_ involves demanding the _artifacts_ from a _provider_ (e.g. `DefaultInfo`). It can be done with `buck2 build fbcode//eden:eden`.

## Coding conventions

Beyond the obvious (well-tested, easy to read) we prefer guidelines that are automatically enforced, e.g. through `rust fmt`, Clippy or the custom linter we have written. Some rules:

* Use the utilities from Gazebo where they are useful, in particular, `dupe`.
* Prefer `to_owned` to convert `&str` to `String`.
* Qualify `anyhow::Result` rather than `use anyhow::Result`.
* Most errors should be returned as `anyhow::Result`. Inspecting errors outside tests and the top-level error handler is strongly discouraged.
* Most errors should be constructed with `thiserror` deriving `enum` values, not raw `anyhow!`.
* We use the `derivative` library to derive the `PartialEq` and `Hash` traits when some fields should be ignored.
* Prefer `use crate::foo::bar` over `use super::bar` or `use crate::foo::*`, apart from test modules which often have `use super::*` at the top.
* Modules should either have submodules or types/functions/constants, but not both.

### Error messages

* Names (of variables, targets, files, etc) should be quoted with backticks,
  e.g. ``Variable `x` not defined``.
* Lists should use square brackets, e.g. ``Available targets: [`aa`, `bb`]``.
* Error messages should start with an upper case letter.
  Error messages should not end with a period.

## License

Buck2 is both MIT and Apache License, Version 2.0 licensed, as found in the [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) files.
