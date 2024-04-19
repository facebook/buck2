---
id: external_cells
title: External Cells
---

Normally, buck2 requires source files to be checked into the repo. However, this
is sometimes inconvenient. It makes distribution of the prelude hard, and users
may want to pull in third party dependencies without vendoring them or using
source control tricks.

To help support these use cases, buck2 has a concept of "external cells."
External cells act much like [normal cells], except that instead of having their
source files checked into the repo, the source files have some alternative
origin.

[normal cells]: ../../concepts/buckconfig.md/#cells

## Setting up an external cell

Configuring an external cell looks much like configuring a regular cell. First,
add the cell to the `cells` section of your `.buckconfig` like normal:

```
[cells]
  prelude = some/path
```

The external cell's files won't actually be generated in the repo. However, you
still need to provide a path for it - this path influences the handling of tree
files, since those cross cell boundaries. It's also used for
`expand-external-cells`, more on that below.

Next, add an entry to the `external_cells` buckconfig section that specifies the
"origin" of the external cell given an alias. This tells buck2 where you want to
get the cell from, if not files in the source repo.

```
[external_cells]
  prelude = bundled
```

For the `bundled` origin, that's it. Other origins may require additional
configuration.

## Origins

Currently, the only supported origin is the `bundled` one. This origin can only
be used with the `prelude` cell, and provides access to a copy of the prelude
that is bundled as part of the buck2 binary. We certainly want to at least
support git repos, and possibly other HTTP endpoints. Ideas or PRs for
extensions are welcome.

## Expanding external cells

Because external cells only represent a different way to access source files,
buck2 provides an `expand-external-cell` command. This command will make a copy
of the external cell into the path in the repo you specified for your cell. By
commenting out the `external_cells` buckconfig entry, this allows you to make
direct edits to the cell's files in your repo.

## Details & Limitations

- External cells can only be configured in the project root's `.buckconfig`.
  This also means that there is no support for "transitive" external cells, ie
  an external cell cannot specify additional external cells to pull in.
- External cells cannot have nested cells inside them.
- The `cells` buckconfig section of external cells is ignored. This is done to
  ensure that when using an external cell to access some dependency in a git
  repo, that git repo can still be an independently building project that
  specifies its own toolchain and prelude configuration.

  Because of this difference between external and non-external cells, it's
  possible that running `buck2 expand-external-cell` may not produce a working
  cell immediately, but instead require you to delete the `cells` section first.

  `cell_aliases` still work just like with regular cells.
