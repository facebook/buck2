# Configuration transitions

Configuration transition is a mechanism for changing the configuration
when depending on a target. Currently we have incoming and outgoing transitions:
* incoming, or per-rule transitions, are declared on the rule
* outgoing, or per-attribute transitions, are declared on the attribute

## Transition rule

Transition rules are defined in `.bzl` files using `transition` builtin.

`transition` function creates a configuration-related object.
`transition` object is opaque, it does not have any operations,
and can only be used as an argument to `rule` function or attribute constructor.

`transition` function call must be assigned to a global variable,
this is similar to user defined provider declaration.

`transition` function takes three arguments:
* `implementation`: function
* `refs`: references to configuration rules to be resolved and passed to implementation function
* optional `split`: `bool` flag (default `False`) to indicate whether transition
  is a split transition (used in per attribute transitions)

Implementation function takes two arguments:
* `platform`: a configuration to transition
* `refs`: resolved references as a struct

Example transition from ios to watchos (for example, to build watchOS bundle
as part of iOS build):

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

Transition function applied twice must produce the configuration identical
to the configuration produced after applying transition once.

```python
assert tr(tr(platform=platform, refs=refs), refs=refs) == tr(platform=platform, refs=refs)
```

If this invariant is not held, certain operations produce incorrect
and possibly infinite graphs.

This is not enforced yet.

## Per rule transition

`rule` function has optional `cfg` attribute, which takes a reference to `transition` object
(created with `transition` function; not a string).

When such rule is called, the rule is instantiated not with the requested configuration,
but with requested configuration transformed with given rule transition.

Example: transition for watchos when ios target depends on watchos resource:

```python
watchos_resource = rule(
    cfg = iphone_to_watch_transition,
    ...
)
```

## Per attribute transition

`attrs` object has two attribute constructors:
* `attrs.transition_dep(cfg)`
* `attrs.split_transition_dep(cfg)`

These attributes are similar to `dep` attribute except for when dependencies are resolved
for the rule instance, they are resolved not with the rule instance configuration,
but with the configuration transformed with given transition.

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

when invoked like:

```python
android_binary(
    deps = ["//foo:bar", "//qux:quux"],
)
```

the rule implementation gets something like this in `deps` attribute:

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

It is an error to pass split transition object to `attrs.transition_dep`
and non-split transition to `attrs.split_transition_dep`.

## Per target transition

We are considering implementing per target transitions
(i.e. transitions referenced at rule instantiation site as opposed to rule declaration site).
No specific plans or API at the moment.

It *could* be something like:

```python
cxx_binary(
    name = "foo",
    cfg = "//transitions:opengl-es-1.0",
    ...
)
```

## Request transition on command line

[RFC](https://www.internalfb.com/diff/D35136639).

## Access rule attributes in transition function implementation

It might be useful for transition function to be able to query rule attributes
(for example, to perform transition to different configurations depending on
`java_version` attribute). This is not implemented.
