---
id: modifiers
title: Configuration modifiers
---

Configuration modifiers (or just "modifiers" for short) are a feature
that lets users add ad-hoc [configuration
constraints](../rule_authors/configurations) to individual
directories, target definitions and individual `buck2` invocations.

## Adding modifiers to a project

To add support for modifiers in your project, simply call `set_cfg_constructor` in your root `PACKAGE` file.

See the [dedicated how-to](../users/how_tos/modifiers_setup) for more
information.

## Modifiers in `PACKAGE` files

Modifiers can be specified per-directory. In that case, they are used as
the default build configurations defined in that directory.

See the [dedicated how-to](../users/how_tos/modifiers_package) for more
information.

## Modifiers on targets

Modifiers can be added to individual targets. This can be used as an
alternative to the missing [per-target
transitions](../rule_authors/configurations#per-target-transition).

See the [dedicated how-to](../users/how_tos/modifiers_target) for more
information.

## Modifiers on the CLI

Custom modifiers can be specified on the CLI to build a target with a
specific build configuration. For example, this can be used to build a
target with a non-default compiler, or to run a test using a specific
sanitizer.

See the [dedicated how-to](../users/how_tos/modifiers_cli) for more
information.

## More information

Official documentation on modifiers is sparse at the moment, so you
might be interested in the following documents:

- [The original RFC](../rfcs/cfg-modifiers/modifiers.pdf)
- [The @ configuration CLI RFC](../rfcs/drafts/configuration-at-syntax)
