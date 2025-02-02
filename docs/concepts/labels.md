---
id: labels
title: Labels
---

The term "label" is a little overloaded in Buck2. It can refer to:

- A target identifier composed of its name, cell and path.
- One or more tag-like strings attached to a test target.

For the first definition of label, the target label is determined by
where it's defined, as well as its `name` attribute. Labels are exposed
in the build API via the
[`Label`](https://buck2.build/docs/api/build/Label/) type, which can be
used in rule implementations and BXL scripts.

For the second definition of label, test rules expose a `label`
attribute, which is a list of strings. Specific labels can be targeted
when running `buck2 test` by adding the `--exclude`/`--include` flags.
See the [test execution
docs](https://buck2.build/docs/rule_authors/test_execution/) for more
information.
