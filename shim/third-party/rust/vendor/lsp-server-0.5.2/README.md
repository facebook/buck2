<div align="center">
  <h1>lsp-server</h1>
  <p style="margin-bottom: 0.5ex;">
    <a href="https://docs.rs/lsp-server"><img src="https://docs.rs/lsp-server/badge.svg" /></a>
    <a href="https://crates.io/crates/lsp-server"><img
        src="https://img.shields.io/crates/v/lsp-server.svg?logo=rust" /></a>
  </p>
  <p style="margin-bottom: 0.5ex;">
    <a href="https://rust-analyzer.github.io/lsp-server"><img
        src="https://img.shields.io/badge/docs-latest-blueviolet?logo=Read-the-docs&logoColor=white" /></a>
    <a href="https://github.com/rust-analyzer/lsp-server/actions"><img
        src="https://github.com/rust-analyzer/lsp-server/workflows/ci/badge.svg" /></a>
    <a href="https://crates.io/crates/lsp-server"><img
        src="https://img.shields.io/librariesio/release/cargo/lsp-server.svg?logo=rust" /></a>
  </p>
  <strong>A language server scaffold exposing a crossbeam-channel API.</strong>
</div>

## Description

This crate is a language server scaffold, exposing a synchronous crossbeam-channel based API. It handles protocol handshaking and parsing messages, while you control the message dispatch loop yourself.

See `examples/goto_def.rs` for a minimal example LSP server that can only respond to the `gotoDefinition` request. To use the example, execute it and then send an `initialize` request.