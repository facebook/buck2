# Starlark VS Code LSP extension

Based on a combination of:

* Tutorial at https://code.visualstudio.com/api/language-extensions/language-server-extension-guide
* Code for the tutorial at https://github.com/microsoft/vscode-extension-samples/tree/master/lsp-sample
* Syntax files from https://github.com/phgn0/vscode-starlark (which are the Microsoft Python ones with minor tweaks)

## Pre-requisites

You need to have npm v7+ installed. Afterwards, run `npm install` in this folder.

## Debugging

- Follow steps in Pre-requisites section.
- Open VS Code on this folder.
- Press Ctrl+Shift+B to compile the client and server.
- Switch to the Debug viewlet.
- Select `Launch Client` from the drop down.
- Run the launch config.

## Installing

- Follow steps in Pre-requisites section.
- Run `npm install vsce`
- Run `npm exec vsce package`
- In VS Code, go to Extensions, click on the "..." button in the Extensions bar, select "Install from VSIX" and then select the `starlark-1.0.0.vsix` file.
- Build Starlark binary and put it on your `$PATH`, e.g. `cargo build --bin=starlark && cp $CARGO_TARGET_DIR/debug/starlark ~/.cargo/bin/starlark`.

## Updating

Every few months security advisories will arrive about pinned versions of packages.

* `npm audit` to see which packages have security updates.
* `npm update` to update the necessary packages.
* `npm exec vsce package` to confirm everything still works.
