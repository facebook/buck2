# How to work with third-party deps?

Use [`go` tool](https://go.dev/doc/modules/managing-dependencies) for that

1. Add/Remove a dependency in your code
1. `cd buck2/prelude/go_bootstrap/tools`
1. `go mod tidy` - to resolve deps
1. `go mod vendor` - to save deps in the repo
