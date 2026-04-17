# rust_doc_merge example

Exercises RFC 3662 mergeable rustdoc support
(`prelude//rust:doc_merge.bxl`). Three crates: `crate_a` (leaf),
`crate_b` (depends on A), and `bin` (depends on both).

Uses the bundled prelude (`[external_cells] prelude = bundled`), so
edits to `../../prelude/` show up after the next `cargo build --bin
buck2`.

## Build per-crate HTML (existing behaviour)

```
buck2 build '//crate_a:crate_a[doc]' --show-output
```

## Produce a single merged HTML tree across all three crates

```
buck2 bxl prelude//rust:doc_merge.bxl:merge -- --targets //...
```

The BXL prints an absolute path to a directory containing merged HTML
with a cross-crate index. Serve it with e.g.:

```bash
env -C "$(buck2 bxl prelude//rust:doc_merge.bxl:merge -- --targets //...)" python3 -m http.server
```

Pass `--include-deps=true` to additionally pull in transitive rust
dependencies of the listed targets.
