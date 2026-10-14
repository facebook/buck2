# retrace — buck2 build-trace explorer

Analyses and a timeline viewer for buck2 builds, over the action trace and
the dependency graph that `buck2 build --log-action-graph` records.

[The honesty contract](docs/contract.md) holds the rules every number
retrace emits follows, and [integration boundaries](docs/integrations.md)
says what retrace depends on.

## Build and test

From the Buck2 workspace:

    cargo build -p retrace
    cargo test -p retrace
