# Hermetic Go Toolchain exampe

## Buckify third-party deps

```sh
$ go mod vendor
$ buck2 run prelude//go/tools/gobuckify:gobuckify -- .
```
