---
id: compilation_database
title: Compilation databases
---

# Generating compilation databases

You can generate compilation databases for consumption by tools such as clangd
and clang-tidy by running the following BXL script:

```sh
buck2 bxl prelude//cxx/tools/compilation_database.bxl:generate -- --targets ...
```

The script will generate a compilation database for all source and header inputs
to the targets listed on the command line. The path to the database is printed
to `stdout`. Note that files that are referenced by multiple targets will have
multiple associated entries in the database, which may not be desirable in all
circumstances. For example, clang-tidy runs analysis for each entry sequentially
when the file being linted has several entries.

It is common to symlink the resulting data at the root of the repository:

```sh
ln -sf $(buck2 bxl prelude//cxx/tools/compilation_database.bxl:generate -- --targets ...) $(git rev-parse --show-toplevel)
```

Since the path to the script is rather long, consider setting up an alias in
your repository:

```python
# `comp_db.bxl`

load("@prelude//cxx/tools/compilation_database.bxl:generate", "generate")

gen = generate
```

```sh
ln -sf $(buck2 bxl comp_db.bxl:gen -- --targets ...) $(git rev-parse --show-toplevel)
```

Providing better ergonomics for BXL scripts (such as enabling something like
`buck2 comp_db`) is being discussed
[here](https://github.com/facebook/buck2/issues/86).
