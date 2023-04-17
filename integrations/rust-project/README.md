# rust-project

The `rust-project` tool is intended to read dependency information from `buck`,
and generate a
[rust-project.json](https://rust-analyzer.github.io/manual.html#non-cargo-based-projects)
file for use with `rust-analyzer`.

The primary motivation for this tool is that there are some projects that fail
to work with `autocargo`. Therefore, to use `rust-analyzer` with those projects,
another solution is needed to provide it with the project structure.

# Usage

To generate a `rust-project.json` file using `rust-project`, supply it with one
or more `buck` targets. Assuming that the current working directory is
`fbcode//common/rust/tools/rust-project`, the following command will create a
`rust-project.json` in the directory corresponding to the above target:

```bash
./fbcode/common/rust/tools/rust-project/rust-project develop fbcode//common/rust/tools/rust-project:rust-project
```

The `develop` command will write to the current working directory.

Placing `rust-project.json` at the root of the Rust project directory will allow
`rust-analyzer`-the-LSP-engine to find and use it for analysis.

To emit logs, set the environment variable `RUST_LOG` to a value. Supported
syntax is described
[here](https://docs.rs/tracing-subscriber/latest/tracing_subscriber/struct.EnvFilter.html).
