<!-- Source: https://docs.google.com/document/d/1GTgFP0vNo67YLE52ypx9qMgJZtDW7Sl3jqx7KPanekw/ -->

# [RFC] Unified `constraint()` Resolution

This tab is the short version of the RFC, focused on the high level API. See the [Shortcomings of existing features tab](problems.md) for why existing approaches fall short, and the [Advanced API tab](advanced_api.md) for the full API surface and advanced use cases.

## Problem

Sometimes the value of one constraint should depend on the values of others. This shows up all over the repo: cpp optimization level depends on build mode, default link style depends on OS and sanitizer, MSVC can't be used outside of windows, and so on.

This RFC uses the NCCL (NVIDIA Collective Communication Library) version constraint as its running example. Our repo supports multiple versions of NCCL, but only the latest version has good sanitizer support, so any sanitizer build should always use the latest NCCL version regardless of what version was otherwise selected.

Suppose NCCL and sanitizer constraints are defined as follows.

```python
# cfg//nccl/BUCK file

constraint(
  name = "version",
  values = ["stable", "beta", "latest"],
  default = "stable",
)

# cfg//BUCK file

constraint(
  name = "sanitizer",
  values = [
    "asan",
    "tsan",
    "ubsan",
    "none",
  ],
  default = "none",
)
```

Existing solutions fall short in several ways (documented in [this tab](problems.md)). This RFC proposes an API that handles this general class of cross-constraint dependency. The NCCL example walks through the API, but the same shape applies to every case above.

## Resolution API

This RFC introduces a new `resolution` attr to the [unified constraint rule](https://docs.google.com/document/d/1AydiiQWBhB_VTl07jPZmyApPkjgxbs4-OQU_nC2Pt9M/edit?tab=t.n2ag5qjff01f#heading=h.f1h19vugfrcm). `resolution` lets users define how a constraint's value should resolve based on the values of other constraints.

To solve the example NCCL problem, resolution on the NCCL version can be defined as follows.

```python
# cfg//nccl/BUCK file

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

The select in `resolution` gets evaluated against the current configuration when the constraint is used as a select key. In this case, selecting on NCCL version always matches `latest` when a sanitizer is enabled. Otherwise, it matches the existing value set for NCCL version constraint in the configuration.

`SELF` is a special keyword that tells `resolution` to use the input value of the constraint as is. `resolution` defaults to `SELF` when unset.

Because `resolution` is evaluated at select time, **no matter how the configuration changes** (ex. modifiers, transitions, default target platform, configured aliases), **latest nccl version on sanitizer builds is always enforced**. This allows constraint authors to define **rules about constraints that cannot be broken** (ex. cannot use MSVC outside of windows).

Additionally, users of nccl don't need to know this special logic exists. They can directly add nccl modifiers or select on the nccl version, and sanitizer variation is already handled for them. This is thanks to resolution encapsulating complex logic at constraint definition site and thus abstracting them away from the constraint user.

A constraint's resolution may depend on resolving other constraints’ resolution. Cycles in dependency are not allowed and will produce an error.

## Input and return values

To expand on how the resolution API works, a constraint now has *two kinds of values: input values and return values*.
* Input values define the list of values that make up a configuration. They get hashed to produce the configuration hash. They can be set via platforms, modifiers, transitions, etc.
* Return values are observed at select evaluation time, at target compatibility evaluation time, and inside `ConfigurationInfo.get()` (in bxl and transitions). In other words, return values are observed when the value of the constraint is evaluated. Return values can only be obtained on a constraint by resolving the `resolution` attr, so all of the above evaluation methods require running resolution.

Specifying `values` is shorthand for setting the same list for both `input_values` and `return_values`. In other words, the earlier NCCL version example could also be re-written as:

```python
constraint(
  name = "version",
  input_values = ["stable", "beta", "latest"],
  return_values = ["stable", "beta", "latest"],
  default = "stable",
  resolution = select({
    "cfg//:sanitizer[asan]": "latest",
    "cfg//:sanitizer[tsan]": "latest",
    "cfg//:sanitizer[ubsan]": "latest",
    "cfg//:sanitizer[none]": "SELF",
  }),
)
```

Every value produced by `resolution`'s select must either be `SELF` or one of `return_values`. Specifying `values` together with `input_values` or `return_values` is an error.

**Input and return values are independent lists.** While they could be identical, they also don't have to overlap at all, and the constraint author is free to give them whatever relationship makes sense. The two subsections below walk through some common scenarios; the [Advanced API RFC](advanced_api.md#interesting-use-cases-of-resolution) covers more patterns.

### Non-matching input and return values

It's also possible to define values for nccl as follows.

```python
# cfg//nccl/BUCK file

constraint(
  name = "version",
  input_values = [
    "stable_or_latest_if_sanitized",
    "beta_or_latest_if_sanitized",
    "latest",
  ],
  return_values = ["stable", "beta", "latest"],
  default = "stable_or_latest_if_sanitized",
  resolution = select({
    "cfg//:sanitizer[asan]": "latest",
    "cfg//:sanitizer[tsan]": "latest",
    "cfg//:sanitizer[ubsan]": "latest",
    "cfg//:sanitizer[none]": select({
      "cfg//nccl:version[stable_or_latest_if_sanitized]": "stable",
      "cfg//nccl:version[beta_or_latest_if_sanitized]": "beta",
      "cfg//nccl:version[latest]": "latest",
    }),
  }),
)
```

Here, `cfg//nccl:version[stable_or_latest_if_sanitized]` can be set as a modifier but can't be used in a select. Likewise, `cfg//nccl:version[stable]` can be used in a select but can't be set as a modifier. Only `cfg//nccl:version[latest]` works in both.

This alternative makes it explicit to users what the nccl version modifiers do, but it is more verbose. I think matching input and return values reads more cleanly for this example.

Note that it's still OK to use `"SELF"` when input and return values do not match in general, however, the specific instance in which it's used must have matching input and return values.

So in other words, given the constraint above where only `"latest"` is in both `input_values` and `return_values`:

```python
resolution = select({
  ...
  "cfg//nccl:version[latest]": "SELF",
}),
```

is fine, but:

```python
resolution = select({
  ...
  "cfg//nccl:version[stable_or_latest_if_sanitized]": "SELF",
}),
```

is an error.


### Empty input values

The earlier nccl `resolution` repeats `"latest"` once per sanitizer key. That's verbose, and adding a new sanitizer means hunting down every select in the repo that branches on it. We can fix this by deriving a new constraint from the sanitizer constraint.

```python
# cfg//BUCK

constraint(
  name = "is_sanitizer_enabled",
  return_values = ["yes", "no"],
  resolution = select({
    "cfg//:sanitizer[asan]": "yes",
    "cfg//:sanitizer[tsan]": "yes",
    "cfg//:sanitizer[ubsan]": "yes",
    "DEFAULT": "no",
  }),
)
```

Now nccl version can branch on `is_sanitizer_enabled` instead of enumerating every sanitizer:

```python
# cfg//nccl/BUCK file

constraint(
  name = "version",
  values = ["stable", "beta", "latest"],
  default = "stable",
  resolution = select({
    "cfg//:is_sanitizer_enabled[yes]": "latest",
    "cfg//:is_sanitizer_enabled[no]": "SELF",
  }),
)
```

Empty `input_values` has three consequences:

- The constraint can't be set as a modifier, transition, configured alias, or by any other input mechanism. Doing so is an error. Its value must be derived from the rest of the configuration.
- `default` is not required and may be `None`, since there are no input values to default to.
- `allow_trivial_constraint` now checks `return_values` count instead of `values`, since a constraint with no input values can still be non-trivial.

## Outlook

The unified `constraint()` rule will be the *only* supported way to express dependency relationships between constraints in the future. Once this feature ships, we'll deprecate two existing mechanisms and work to remove them:

- **Conditional modifiers.** Resolution subsumes their use case and doesn't suffer from the override problem documented in the [problems tab](problems.md#case-2-use-conditional-modifiers).
- **`select()` inside transition attrs.** Today these have to be double-resolved (pre and post transition) and the build fails if the two resolutions diverge. Resolution makes them unnecessary by pulling the conditional logic into the constraint itself, so the framework can stop supporting them.
