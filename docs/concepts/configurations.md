---
id: configurations
title: Configurations
---

For rule authors see also: [Configurations](../rule_authors/configurations.md)

When building a target, buck always builds it in a particular "configuration."
The configuration typically includes information like the target os, target
arch, sanitizers, opt level, etc. One way to understand the effect that a
configuration has is via the `cquery` and `uquery` commands. The cquery command
will compute the appropriate configuration for a target and display a version of
that target's attributes with the configuration applied. The `uquery` command
will not apply a configuration.

Here is a heavily trimmed version of the outputs of invoking `uquery` and
`cquery` on `//buck2/app/buck2_core:buck2_core`.

```sh
> buck2 uquery -A '"//buck2/app/buck2_core:buck2_core"'
{
  "fbcode//buck2/app/buck2_core:buck2_core": {
    "buck.type": "rust_library",
    "buck.package": "fbcode//buck2/app/buck2_core:TARGETS",
    "name": "buck2_core",
    "visibility": [
      "PUBLIC"
    ],
    "deps": {
      "fbsource//third-party/rust:anyhow",
      "fbsource//third-party/rust:arc-swap",
      "fbsource//third-party/rust:blake3",
      "fbsource//third-party/rust:compact_str",
      "fbsource//third-party/rust:dashmap",
      {
        "__type": "selector",
        "entries": {
          "DEFAULT": [],
          "ovr_config//os:windows": [
            "fbsource//third-party/rust:common-path"
          ]
        }
      },
      {
        "__type": "selector",
        "entries": {
          "DEFAULT": [],
          "ovr_config//os:linux": [
            "fbsource//third-party/rust:nix"
          ]
        }
      },
    },
  }
}
```

```sh
> buck2 cquery -A '"//buck2/app/buck2_core:buck2_core"'
{
  "fbcode//buck2/app/buck2_core:buck2_core (ovr_config//platform/linux:<OMITTED>)": {
    "buck.type": "rust_library",
    "buck.package": "fbcode//buck2/app/buck2_core:TARGETS",
    "buck.target_configuration": "ovr_config//platform/linux:<OMITTED>",
    "buck.execution_platform": "fbcode//buck2/platform/<OMITTED>",
    "name": "buck2_core",
    "visibility": [
      "PUBLIC"
    ],
    "deps": [
      "fbsource//third-party/rust:anyhow (ovr_config//platform/linux:<OMITTED>)",
      "fbsource//third-party/rust:arc-swap (ovr_config//platform/linux:<OMITTED>)",
      "fbsource//third-party/rust:blake3 (ovr_config//platform/linux:<OMITTED>)",
      "fbsource//third-party/rust:compact_str (ovr_config//platform/linux:<OMITTED>)",
      "fbsource//third-party/rust:dashmap (ovr_config//platform/linux:<OMITTED>)",
      "fbsource//third-party/rust:nix (ovr_config//platform/linux:<OMITTED>)"
    ]
}
```

The `cquery` output has additional `buck.target_configuration` and
`buck.execution_platform` attributes which tell you what the target is being
built for and what it's being built on, respectively. `uquery` doesn't have
those.

The deps in `uquery` also have a number of selects; these indicate that the
`common-path` dependency should only be included when building for Windows,
while the `nix` dependency is needed only for Linux. In `cquery` that
distinction has been resolved; because the target has been configured for Linux,
the `nix` dependency is present and indistinguishable from any other, while the
`common-path` dependency is gone.
