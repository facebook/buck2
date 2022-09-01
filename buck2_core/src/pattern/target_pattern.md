A __target pattern__ is a string that describes a set of one or more targets. You can use target patterns as arguments to commands, such as buck build and buck query. You can also use target patterns in the Visibility argument of your build rules.

The simplest target pattern `//apps/myapp:app` matches exactly the target of the same name `//apps/myapp:app`.


A target pattern that ends with a colon matches all targets in the build file at the preceding directory path. For example, suppose that the build file `apps/myapp/BUCK` defines the rules: app_debug and app_release, then the target pattern `//apps/myapp:` matches `//apps/myapp:app_debug` and `//apps/myapp:app_release`.


A target pattern that ends with an ellipsis "/..." matches all targets in the build file in the directory that precedes the ellipsis and also all targets in build files in subdirectories (within the same cell). For example, suppose that you have the following build files: `apps/BUCK`, `apps/myapp/BUCK`. Then the target pattern `//apps/...` would match (for example) `//apps:common` and `//apps/myapp:app`. The pattern `//...` would match the same (even though there's no build file in the root directory).

## Cell resolution

Cells will be resolved in the context where the target pattern appears. When used as arguments to the command line, they will be resolved based on the cell of the directory in which the command is invoked.

If `~/project` and `~/project/cell` are both cells with names `project` and `cell` respectively, then `//some:target` would resolve to `project//some:target` if it appears in `~/project/BUCK` and `cell//some:target` if it appears in `~/project/cell/BUCK`.

## Relative patterns

Target patterns can be absolute (`//my/app:target`, `cell//other:target`) or relative `app:target`. A relative pattern will be resolved relative to the working directory of the command.

## Restrictions

A target pattern should not include any `..` segments. This applies to both absolute and relative patterns.

## Inner Providers
A target pattern used where providers labels are expected can refer to rule's inner providers via `//my/app:target[<providers-label>]` syntax.
The inner providers label will refer to a specific set of providers exposed by a rule, such as a particular set of outputs from the rule.

The providers label can be used for commands: buck builds, provider queries, and action queries.
Any rule's dependencies also refers to a providers label.
However, configuration rules (i.e config_settings) should be referred to without providers.
(TODO: experiment and see if we should just use provider labels everywhere).

The providers label syntax can only be used when the pattern is of a specific rule. Package and recursive patterns (e.g. `//some/pkg:` and `//some/...`) cannot have providers labels.
