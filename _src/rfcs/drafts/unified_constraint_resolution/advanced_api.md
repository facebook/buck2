# Advanced API

This tab covers the rest of the API surface of the RFC, common use cases for the input/return value split, and migration concerns. Read the [RFC at a glance tab](main.md) first for the problem statement and the high level `resolution` API. Everything below builds on that foundation.

## `ConfigurationInfo` and `PlatformInfo` API changes

`ConfigurationInfo` and `PlatformInfo` expose `.constraints` as a flat dict, plus `.get()`, `.insert()`, and `.pop()` that fall back to the constraint's `default` today. Once `resolution` exists, these need defined semantics:

- `.get(constraint)` returns the resolved (return) value, via a new provider separate from `ConstraintValueInfo` (return values will produce different providers than input values).
- `.insert(constraint, value)` writes an input value. `value` continues to be `ConstraintValueInfo`.
- `.pop(constraint)` removes and returns the input value, or returns the constraint's default if it wasn't set.
- Iteration over `.constraints` yields input values (as `ConstraintValueInfo`).

As part of the unified `constraint()` rule rollout, we'll migrate transition functions off raw `.constraints` access and remove the raw access path.

## `constraint_resolution` rule

One UX problem with `resolution` is that it requires all resolution logic to live inline in the constraint definition. On popular constraints like cpp optimization level, this can balloon into long, complex selects that need to handle different variations across product types in the repo.

To avoid hard-to-read selects, we'll introduce a `constraint_resolution` rule. It lets you factor a chunk of resolution logic into its own target and reference it from `resolution`. For example, cpp optimization level could look like:

```python
# cfg//cpp/BUCK

constraint(
  name = "opt_level",
  values = ["o0", "o1", "o2", "o3"],
  default = "o3",
  resolution = select({
    "cfg//:project[foo]": "root//project_foo/cfg:opt_level",
    # plus other project-specific overrides
    "DEFAULT": select({
      "cfg//:build_mode[dev]": "o0",
      "DEFAULT": "o3",
    }),
  }),
)

# root//project_foo/cfg/BUCK

constraint_resolution(
  name = "opt_level",
  constraint = "cfg//cpp:opt_level",
  resolution = select({
    "cfg//:build_mode[dev]": "o1",
    "DEFAULT": "o3",
  }),
)
```

Plugging in project foo's resolution only takes one extra line in the `opt_level` constraint definition. All the project foo specific logic for cpp optimization level lives in project foo directly, which is nicer from both UX and code ownership perspectives.

## Default values

`default` currently is a special subtarget on `constraint`, such that you can use `cfg//:sanitizer[default]` to reference `cfg//:sanitizer[none]` when setting modifiers. We'll rename this subtarget from `default` to `DEFAULT` to make it obvious it's a special keyword.

You may have seen plans for `default` to support selects in a previous iteration of the unified `constraint()` rule RFC. That is no longer planned.

Additionally, we'll make a configuration with the default value set equivalent to a configuration with the constraint unset. Concretely, with the sanitizer constraint from the [RFC at a glance tab](main.md) (`default = "none"`), a configuration with `cfg//:sanitizer[none]` will be identical to a configuration without the sanitizer constraint set at all. Beyond simplifying UX, this should produce some capacity savings for CI. Today, several constraints create distinct configurations depending on whether the default value is set explicitly, which costs Buck extra memory at configuration time. This change also requires full rollout of the unified `constraint()` rule.

## Interesting Use Cases of Resolution

### Input values as a superset of return values

There are also use cases where you want more inputs than return values. For example, in the NCCL example, certain projects may not want to bump their NCCL version when sanitizer is enabled. This can be supported by adding pin values that hold the NCCL version regardless of sanitizer state.

```python
# cfg//nccl/BUCK file

constraint(
  name = "version",
  input_values = [
    "stable",
    "beta",
    "latest",
    "always_stable",
    "always_beta",
  ],
  return_values = ["stable", "beta", "latest"],
  default = "stable",
  resolution = select({
    "cfg//nccl:version[always_stable]": "stable",
    "cfg//nccl:version[always_beta]": "beta",
    "cfg//nccl:version[latest]": "latest",
    # Note: this works because selects now resolves based on first match
    "cfg//:sanitizer[asan]": "latest",
    "cfg//:sanitizer[tsan]": "latest",
    "cfg//:sanitizer[ubsan]": "latest",
    "cfg//:sanitizer[none]": "SELF",
  }),
)
```

### select_fail/select_incompatible

NCCL only works on linux. The cleanest way to encode that in `resolution` is to fail when the OS isn't linux, using `select_fail` or `select_incompatible` (both supported in `resolution`).

```python
# cfg//nccl/BUCK file

constraint(
  name = "version",
  values = ["stable", "beta", "latest"],
  default = "stable",
  resolution = select({
    "cfg//:os[linux]": select({
      "cfg//:is_sanitizer_enabled[yes]": "latest",
      "cfg//:is_sanitizer_enabled[no]": "SELF",
    }),
    "DEFAULT": select_incompatible("nccl incompatible with non-linux OS"),
  }),
)
```

This lets users concentrate incompatibilities between constraints at a single location, rather than spreading them across every relevant target in the repo.

### A "not applicable" return value

Suppose we want to record the version of NCCL used to build a binary directly into the binary for auditing purposes (internally known as "build info"). With `select_fail` or `select_incompatible`, reading NCCL version on a non-linux build would fail outright, but for build info, we'd rather not fail on non-relevant configurations. We can add a `not_applicable` return value instead:

```python
# cfg//nccl/BUCK file

constraint(
  name = "version",
  input_values = ["stable", "beta", "latest"],
  return_values = [
    "stable",
    "beta",
    "latest",
    "not_applicable",
  ],
  default = "stable",
  resolution = select({
    "cfg//:os[linux]": select({
      "cfg//:is_sanitizer_enabled[yes]": "latest",
      "cfg//:is_sanitizer_enabled[no]": "SELF",
    }),
    "DEFAULT": "not_applicable",
  }),
)
```

In practice, this protects users from reading the NCCL version in cases where it's not used and not relevant. When the OS isn't linux, NCCL version always resolves to `not_applicable`.

There are tradeoffs between the `select_fail`/`select_incompatible` approach and `not_applicable` approach. `select_incompatible` makes mistakes loud, but it also means every select on NCCL version has to be exclusively for Linux to stay compatible. In practice, every select on NCCL version may need to select on linux first. `not_applicable` keeps consumer selects simple at the cost of not failing immediately. My personal opinion is to use `not_applicable` when a select should just ignore nccl version when it's not relevant, and use `select_incompatible` in places where the combination should error explicitly (e.g. directly on the alias target for nccl). If every place using the select is OK with erroring, `select_fail`/`select_incompatible` is better.

### Conditional defaults

Internally, we like to enable ASAN on linux dev mode by default to catch bugs. However, many projects are incompatible with ASAN, so users are welcome to override that behavior for their projects to build with no sanitizer. Encoding this in the sanitizer constraint requires a dedicated input value for the default behavior.

```python
# cfg//BUCK

constraint(
  name = "sanitizer",
  input_values = ["asan", "tsan", "ubsan", "none", "default"],
  return_values = ["asan", "tsan", "ubsan", "none"],
  default = "default",
  resolution = select({
    "cfg//:sanitizer[default]": select({
      "cfg//:build_mode[dev]": select({
        "cfg//:os[linux]": "asan",
        "DEFAULT": "none",
      }),
      "DEFAULT": "none",
    }),
    "DEFAULT": "SELF",
  }),
)
```

When the `"default"` input value is specified, users get this conditional default behavior for sanitizer. They can opt out by setting any sanitizer constraint as a modifier for their project, or opt back in by setting `cfg//:sanitizer[default]` as a modifier.

See [Sub-constraints](#sub-constraints) for an alternative that hides the conditional-default toggle from the constraint's public input values.

## Sub-constraints

A sub-constraint adds an extra input dimension to a constraint, scoped entirely to that constraint's `resolution`. This is useful when resolution logic needs a knob that doesn't make sense to expose to the rest of the repo as its own constraint.

Sub-constraints have tighter semantics than regular constraints:

- A sub-constraint has only `values` and `default`. It does not have separate `input_values` and `return_values`, and it does not have its own `resolution`.
- A sub-constraint is set as a modifier via the path syntax `cfg//:<parent>/<sub_constraint>[<value>]`. The reported constraint setting label is `cfg//:<parent>/<sub_constraint>`.
- A sub-constraint is invisible to everything outside its parent's `resolution`. It cannot be referenced in any other constraint's `resolution`, any `select` outside the parent's `resolution`, any `constraint_resolution` target, transitions, configured aliases, or platforms. The path label only resolves inside the parent constraint's own resolution.

Sub-constraints will be implemented in the unified `constraint()` rule as separate constraint targets. I haven't decided how to implement the private aspects of `sub_constraints`, but it will most likely be a re-usable attr that you can use to mark a constraint as private to its parent's `resolution`.

Here's how the earlier [conditional sanitizer default](#conditional-defaults) could expose an opt-out toggle via a sub-constraint:

```python
# cfg//BUCK

constraint(
  name = "sanitizer",
  input_values = ["asan", "tsan", "ubsan", "none", "default"],
  sub_constraints = {
    "enable_asan_on_linux_dev": sub_constraint(
      values = ["yes", "no"],
      default = "yes",
    ),
  },
  return_values = ["asan", "tsan", "ubsan", "none"],
  default = "default",
  resolution = select({
    "cfg//:sanitizer[default]": select({
      "cfg//:sanitizer/enable_asan_on_linux_dev[no]": "SELF",
      "cfg//:sanitizer/enable_asan_on_linux_dev[yes]": select({
        "cfg//:build_mode[dev]": select({
          "cfg//:os[linux]": "asan",
          "DEFAULT": "none",
        }),
        "DEFAULT": "none",
      }),
    }),
    "DEFAULT": "SELF",
  }),
)
```

Setting `cfg//:sanitizer/enable_asan_on_linux_dev[no]` as a modifier (e.g. in a project's PACKAGE file) opts that project out of the linux dev asan default without otherwise changing sanitizer behavior.

### Replacing selects in transition attrs

Selects inside transition attrs are another way to express conditionality, but they have the same shortcomings as the [transition-based approaches in the problems tab](problems.md#case-3-use-a-transition-function): anyone writing their own transition can silently override the conditional, and library targets don't pick up the transition at all. Sub-constraints let you push the per-OS or per-config branching into the constraint itself, so the transition reads the constraint directly and the select disappears.

Take the Apple SDK target version. Today `apple_bundle` carries a `minimum_os_version` attr set with a select:

```python
apple_bundle(
  minimum_os_version = select({
    "ovr_config//os:iphoneos": "14.0",
    "ovr_config//os:macos": "12.0",
  }),
)
```

`target_sdk_version_transition` (in `prelude/apple/user/target_sdk_version_transition.bzl`) reads `attrs.minimum_os_version` and writes the `target-sdk-version` constraint. The per-OS branching can move into the constraint via a sub-constraint per OS:

```python
# cfg//apple/BUCK

constraint(
  name = "version",
  input_values = ["v14", "v15", "v16", "default"],
  return_values = ["v14", "v15", "v16"],
  default = "default",
  sub_constraints = {
    "min_ios":   sub_constraint(values = ["v14", "v15", "v16"], default = "v14"),
    "min_macos": sub_constraint(values = ["v12", "v13"],        default = "v12"),
    # min_watchos, min_tvos, ...
  },
  resolution = select({
    "cfg//apple:version[default]": select({
      "cfg//os:iphoneos": select({
        "cfg//apple:version/min_ios[v14]": "v14",
        "cfg//apple:version/min_ios[v15]": "v15",
        "cfg//apple:version/min_ios[v16]": "v16",
      }),
      "cfg//os:macos": select({
        "cfg//apple:version/min_macos[v12]": "v12",
        "cfg//apple:version/min_macos[v13]": "v13",
      }),
    }),
    "DEFAULT": "SELF",
  }),
)
```

The bundle rule drops `minimum_os_version` entirely. PACKAGE files set `cfg//apple:version/min_ios[v15]` etc. as modifiers when they need to override the per-OS minimum. The select inside the transition attr (and the verification machinery that double-resolves it) goes away.

### Rolling out new resolution behavior incrementally

A common conditional-modifier workflow is gating a new behavior behind a constraint, flipping it on per PACKAGE, and deleting the gate once rollout is done. Sub-constraints capture the same workflow without polluting the public constraint namespace. Define a rollout sub-constraint on the constraint that's getting the new behavior, default to `off`, and gate the new resolution branch on it. PACKAGE files opt in by setting the sub-constraint as a modifier. After full rollout, delete the sub-constraint and inline the new branch.

For example, gating the [NCCL latest-when-sanitized resolution](main.md#resolution-api) behind a rollout sub-constraint:

```python
# cfg//nccl/BUCK file

constraint(
  name = "version",
  values = ["stable", "beta", "latest"],
  default = "stable",
  sub_constraints = {
    "use_latest_when_sanitized": sub_constraint(values = ["on", "off"], default = "off"),
  },
  resolution = select({
    "cfg//nccl:version/use_latest_when_sanitized[on]": select({
      "cfg//:is_sanitizer_enabled[yes]": "latest",
      "DEFAULT": "SELF",
    }),
    "DEFAULT": "SELF",
  }),
)
```

PACKAGE files that want to opt in set `cfg//nccl:version/use_latest_when_sanitized[on]`. Once every project has opted in, the sub-constraint goes away and the new branch becomes unconditional.

Sub-constraints are like private fields of an object in an object-oriented programming model. Reach for sub-constraints when an extra input dimension is meaningful only inside one constraint's resolution and shouldn't leak into the repo's public API. If the dimension is useful to other selects, define a normal constraint instead.

## Migration/Deprecation

### Select resolution semantics

Today, `select` picks the most "refined" key when multiple keys match. For example, if a configuration matches both `linux` and `linux-arm64`, `select` resolves to `linux-arm64`.

To support `resolution`, we need to change how select resolves on multiple matches. Two proposals are on the table.

1. Change `select` to match the *first* key instead of the most refined key. This is our preferred solution. We think refinement encourages bad select definitions and is unintuitive for users.
2. Support a different definition of refinement. For example, count the number of unique constraint matches inside `resolution` as the refinement metric. We don't like this in general and are only considering it as a temporary migration step to unblock `resolution` before moving to #1.

## Alternative Designs

### Input and return values as separate targets

The unified `constraint()` rule packs two concerns into one target: input values (with `default`) and return values (with `resolution`). An alternative is to model them as two targets, one for the input value and one for the return value.

#### Variant A: Same rule, two targets

Keep the existing `constraint()` rule. The author writes two targets, one for the input value and one for the resolved return value. Because they are independent constraints, the return target's `resolution` selects on the input target directly.

```python
# cfg//nccl/BUCK

constraint(
  name = "version_modifier",
  values = ["stable", "beta", "latest"],
  default = "stable",
  select_visibility = [":version"],
)

constraint(
  name = "version",
  values = ["stable", "beta", "latest"],
  default = select({
    "cfg//:sanitizer[asan]": "latest",
    "cfg//:sanitizer[tsan]": "latest",
    "cfg//:sanitizer[ubsan]": "latest",
    "cfg//:sanitizer[none]": select({
      "cfg//nccl:version_modifier[stable]": "stable",
      "cfg//nccl:version_modifier[beta]": "beta",
      "cfg//nccl:version_modifier[latest]": "latest",
    }),
  }),
  default_only = true,
  select_visibility = ["PUBLIC"],
)
```

Modifiers and platforms set `cfg//nccl:version_modifier[stable]`. Selects use `cfg//nccl:version[stable]`. `select_visibility` is a new attr that controls which targets can reference a constraint's values in `select()` expressions, so `version_modifier` here can only be selected on from `version`.

The inner select on `version_modifier` is boilerplate that essentially forwards the input constraint. We can introduce a `constraint_forward` function (analogous to `SELF` in the unified rule) to collapse it:

```python
constraint(
  name = "version",
  values = ["stable", "beta", "latest"],
  default = select({
    "cfg//:sanitizer[asan]": "latest",
    "cfg//:sanitizer[tsan]": "latest",
    "cfg//:sanitizer[ubsan]": "latest",
    "cfg//:sanitizer[none]": constraint_forward(":version_modifier"),
  }),
  default_only = True,
  select_visibility = ["PUBLIC"],
)
```

This variant can achieve similar functionality to the current RFC without introducing separate `input_values` and `return_values`. Many constraints don't need dedicated resolution, and in this variant a constraint without resolution stays as a single `constraint()` target.

The tradeoff is that nothing at the rule level encourages constraint authors to write constraints in this way. The "input vs return" distinction relies entirely on understanding proper usage patterns, and many constraint authors likely don't understand these nuances.

One particular aspect I found problematic was that `select_visibility` is entirely opt-in, so nothing prevents users from bypassing the input/return separation. Two scenarios I'm concerned will happen:
1. Users don't define `select_visibility` and let both `version_modifier` and `version` have public select visibility. A non-trivial amount of Buck users will put in only the minimal effort to make things work, and won't use guardrail features if they aren't required.
2. A user realizes that `version_modifier` can't be selected on and decides the way to fix it is by expanding `select_visibility` of `version_modifier` rather than selecting on `version`, which breaks the benefit of this API in the first place.

Variants A and B share additional pros and cons listed at the end of this section.

#### Variant B: Separate rules for `input_values` and `return values`

Introduce a dedicated pair of rules. `input_constraint()` owns `values` and `default`; `resolved_constraint()` owns `resolution` and its own `return_values`.

```python
# cfg//nccl/BUCK

input_constraint(
  name = "version_modifiers",
  values = ["stable", "beta", "latest"],
  default = "stable",
  resolved_constraint = ":version",
)

resolved_constraint(
  name = "version",
  input = ":version_modifiers",
  return_values = ["stable", "beta", "latest"],
  resolution = select({
    "cfg//:sanitizer[asan]": "latest",
    "cfg//:sanitizer[tsan]": "latest",
    "cfg//:sanitizer[ubsan]": "latest",
    "cfg//:sanitizer[none]": "FROM_INPUT",
  }),
)
```

`FROM_INPUT` is a forwarding placeholder similar to `SELF`.

The rule split makes the role of each target unambiguous. `input_constraint` has no `resolution`, `resolved_constraint` has no `default`, and misuse fails at configuration time instead of producing surprising behavior. `input_constraint` can be set as modifiers but cannot be selected on. `resolved_constraint` can be selected on but cannot be set as modifiers. Compared to variant A, requiring users to set `resolved_constraint` on `input_constraint` is also a much better mechanism for enforcing the encapsulation aspects of the API, as this can enforce that one `input_constraint` can only be selected on by exactly one `resolved_constraint`.

The tradeoff is that this API requires splitting every existing constraint target into two, which means migrating all callsites, either all selects or all modifiers, to the appropriate new target. At our repo's scale, this is infeasible in practice.

#### General pros of variants A and B

- Easier mental model for constraint authors. Each target has one job: the input target holds the modifier-settable value, the return target produces the resolved value.
- Removes the `SELF` keyword and the input/return value overlap rules that the unified design needs.

#### General cons of variants A and B

- Significant friction to adopt `resolution`. Splitting an existing constraint into two targets means migrating every callsite, either all selects or all modifiers, to the new target. Even with AI codemods, this is non-trivial enough to deter adoption in cases where resolution would actually be incredibly useful.
- Trivial use cases that just need resolution without separate `input_values` and `return_values`, like expressing incompatibilities between constraints, become significantly more complicated.
- Two targets for one logical constraint is more confusing for general Buck users on which to use for selects and which to use for modifiers. This group is an order of magnitude larger than constraint authors.

### Variant C: Resolution without separate `input_values` and `return_values`

This is essentially the RFC I'm suggesting, except `constraint()` only has `values` (shared between inputs and returns), not separate `input_values` and `return_values`. Use cases that need different input and return values fall back to either separate constraints or extending values directly.

```python
# cfg//nccl/BUCK

constraint(
  name = "version",
  values = ["stable", "beta", "latest"],
  default = "stable",
  resolution = select({
    "cfg//:sanitizer[asan]": "latest",
    "cfg//:sanitizer[tsan]": "latest",
    "cfg//:sanitizer[ubsan]": "latest",
    "cfg//:sanitizer[none]": "SELF",
  }),
)
```

The pro is that this is simpler than the RFC as presented. There are no separate `input_values` and `return_values` attrs, no rules around which values can appear where, and no `SELF` semantics tied to input/return overlap. For constraints where all values are both inputs and returns (the common case), there's nothing extra to learn. It's also clearer what `default` does: it's the single default value for the constraint, with no question of whether it applies to inputs, returns, or both.

The con shows up when input and return value sets need to diverge. Take the [conditional sanitizer default](#conditional-defaults) from earlier, rewritten in this variant:

```python
# cfg//BUCK

constraint(
  name = "sanitizer",
  values = ["asan", "tsan", "ubsan", "none", "default"],
  default = "default",
  resolution = select({
    "cfg//:sanitizer[default]": select({
      "cfg//:build_mode[dev]": select({
        "cfg//:os[linux]": "asan",
        "DEFAULT": "none",
      }),
      "DEFAULT": "none",
    }),
    "DEFAULT": "SELF",
  }),
)
```

`"default"` is an input value that resolution always converts into `"asan"`, `"tsan"`, `"ubsan"`, or `"none"`. Selecting on `cfg//:sanitizer[default]` is meaningless because the configuration will never carry that value after resolution runs. But because `"default"` lives in `values`, the select is valid syntax. The [`not_applicable` return value](#a-not-applicable-return-value) has the mirror problem: it only makes sense as a return value, but with a flat `values` list, users can also set it as a modifier, which doesn't make sense either. This may deter some constraint authors from reaching for resolution for use cases like default and not applicable when they'd otherwise benefit from using it.

Overall, I'm also quite happy with this variant and think it's also a good approach.
