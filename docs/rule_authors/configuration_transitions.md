---
id: configuration_transitions
title: Configuration Transitions
---

Configuration transition is a mechanism for changing the configuration when depending on a target.

Currently, Buck2 has incoming and outgoing transitions:

* **Incoming** - (or per-rule transitions) declared on the rule.
* **Outgoing** - (or per-attribute transitions) declared on the attribute.

## Transition rule

Transition rules are defined in `.bzl` files using the `transition` built-in.

The `transition` function creates a configuration-related object.
The `transition` object is opaque, it does not have any operations, and can only be used as an argument to `rule` function or attribute constructor.
The `transition` function call must be assigned to a global variable (this is similar to user-defined provider declarations).

The `transition` function takes three arguments:

* `implementation` - a function.
* `refs` - references to configuration rules to be resolved and passed to the implementation function.
* `split` - (optional) `bool` flag (default `False`) to indicate whether transition is a split transition (used in per attribute transitions).

The `implementation` function takes two arguments:

* `platform` - a configuration to transition.
* `refs` - resolved references as a struct.

Example transition from ios to watchos (for example, to build a watchOS bundle as part of an iOS build):

```python
def _impl(platform: PlatformInfo.type, refs: struct.type) -> PlatformInfo.type:
    # Operating system constraint setting.
    os = refs.os[ConstraintSettingInfo]
    # Watchos constraint value.
    watchos = refs.watchos[ConstraintValueInfo]
    # Remove operating system constraint from input platform.
    constraints = {
        s: v
        for (s, v) in platform.configuration.constraints.items()
        if s != os.label
    }
    # Add watchos constraint value.
    constraints[watchos.setting.label] = watchos
    # Construct configuration structure.
    new_cfg = ConfigurationInfo(
        # Updated constraints.
        constraints = constraints,
        # Keep original config values.
        values = platform.configuration.values,
    )
    # And return new configuration,
    # or a dict of marker to configuration in case of split transition.
    return PlatformInfo(
        # ... supplying configuration label.
        label = "<transitioned-to-watch>",
        configuration = new_cfg,
    )

iphone_to_watch_transition = transition(_impl, refs = {
    "os": "//constraints:os",
    "watchos": "//constraints:watchos",
})
```

A transition function applied twice must produce the configuration identical to the configuration produced after applying transition once.

```python
assert tr(tr(platform=platform, refs=refs), refs=refs) == tr(platform=platform, refs=refs)
```

If this invariant is not held, certain operations produce incorrect and possibly infinite graphs. This is not yet enforced.

## Per rule transition

The `rule` function has an optional `cfg` attribute, which takes a reference to the `transition` object (created with the `transition` function; not a string).

When such a rule is called, it is instantiated, not with the requested configuration, but with the requested configuration transformed with a given rule transition.

For example, the transition for watchos when the iOS target depends on watchos resource:

```python
watchos_resource = rule(
    cfg = iphone_to_watch_transition,
    ...
)
```

## Per attribute transition

The `attrs` object has two attribute constructors:

* `attrs.transition_dep(cfg)`
* `attrs.split_transition_dep(cfg)`

These attributes are similar to the `dep` attribute. When dependencies are resolved for the rule instance, then they are resolved not with the rule instance configuration,
but with the configuration transformed with the given transition.

For split transition, each dependency is resolved into a dict of marker to providers.

For example:

```python
android_binary = rule(
    ...
    attrs = {
        "deps": attrs.list(attrs.split_transition_dep(cfg = cpu_split_transition), default = []),
    },
)
```

When the above is invoked as follows:

```python
android_binary(
    deps = ["//foo:bar", "//qux:quux"],
)
```

Then the rule implementation gets something like the following in the `deps` attribute:

```python
{
    [
        {
            # Key in this dict is the marker returned from split transition impl function.
            "arm64": "providers for //foo:bar configured for arm64",
            "armv7": "providers for //foo:bar configured for armv7",
        },
        {
            "arm64": "providers for //qux:quux configured for arm64",
            "armv7": "providers for //qux:quux configured for armv7",
        },
    ]
}
```

:::note
It is an error to pass a split transition object to `attrs.transition_dep` and a non-split transition to `attrs.split_transition_dep`.
:::

## Per target transition

The Buck2 team is considering the implementation of per target transitions (that is, transitions referenced at a rule instantiation site as opposed to rule declaration site).
No specific plans or APIs exists at the moment.

It *could* be something like the following:

```python
cxx_binary(
    name = "foo",
    cfg = "//transitions:opengl-es-1.0",
    ...
)
```

## Request transition on command line

For information, see [RFC](../rfcs/drafts/configuration-at-syntax.md).

## Access rule attributes in transition function implementation

It might be useful for the transition function to be able to query rule attributes (for example, to perform transition to different configurations depending on `java_version` attribute).

Both incoming (per rule) and outgoing (per dependency) transitions can access rule attributes. For outgoing transitions, transition rule implementation accesses the attributes of the target that has dependencies with transitions, not attributes of dependency targets.

```python
def _tr(platform, refs, attrs):
    # NB: There are some restrictions on what attrs can be made accessible:
    # - Only primitive values for now (providers are not resolved)
    # - Only unconfigured attributes for now
    attrs.my_list_attribute # == [12345, 67890]

tr = transition(
  _tr,
  refs = {},
  attrs = {
    "my_list_attribute": attr.list(...),
  },
)

my_rule = rule(..., cfg=tr)

my_rule(
  ...,
  my_list_attribute = [12345, 67890],
)
```
