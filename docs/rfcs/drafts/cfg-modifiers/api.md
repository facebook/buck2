# [RFC] Configuration Modifiers

Target platform is Buck team's endorsed way of supporting custom build
settings, and it's intended to replace the buckconfigs and modefiles of
buckconfigs widely used today. Buckconfigs are bad because they are
global flags, and changing them can cause invalidation of most if not
all of Buck's state, whereas target platforms/configurations are
per-target build settings and allow for scalable multi-configuration
builds like building in multiple settings (ex. dev and opt) concurrently
or modifying the configuration within the dependency graph. However,
despite improvements to configurations in Buck2, buckconfigs continue to
be the predominant way of representing build settings, and they are not
losing popularity - almost all build settings in the repo today have
some sort of buckconfig toggle. This means that users often hit
invalidation overhead from buck2 when switching modefiles or
buckconfigs.

When comparing buckconfigs and target platforms, the biggest usability
problem with target platforms is that **they do not compose well**.

1. *Using target platforms require generating all possible permutations
   of build settings*. As an example, say I want to customize targets
   based on different OSes (linux, mac, or windows), sanitizers (asan,
   tsan, or nosan), and link style (static or shared). With target
   platforms, I need to first generate all possible permutations of
   these settings as platform targets before I can use them. For this
   example, I would need 18 different platforms: linux-asan-static,
   linux-asan-shared, linux-tsan-static, linux-tsan-shared, so on and so
   forth. The exponential nature of platform generation makes it
   challenging to add a new constraint because you could double the
   number of platforms. Buckconfigs don't have this problem because OS,
   sanitizer, and link style can be separate buckconfigs and you do not
   have to generate all possible combinations to use them.

2. *Setting a default target platform is complex.* Setting a default
   target platform for a target requires knowing the exact set of build
   settings needed and the exact platform corresponding to those
   settings. Because a lot of build defaults are defined in bzl files,
   this often ends up getting handled by a set of fairly convoluted bzl
   files many hundreds of lines long. For example, suppose python
   library `root//:foo` prefers the default settings of mac, asan, and
   static. The python library macros must gather those settings at once
   and map them to the platform name `mac-asan-static`. Setting a
   buckconfig as default is as easy as adding the buckconfig value to
   .buckconfig file or a modefile.

3. *You must specify a full target platform on the command line
   for `--target-platforms` flag*. Despite all the convoluted macros
   needed to set a `default_target_platform`, they become useless when
   you don't want the default because you have to specify a **full**
   target platform on the command line. Following the previous example,
   suppose `repo//:foo` defaults to nosan but I want to
   build `repo//:foo` with asan. For target platform, I have to know
   what OS and link style to build `//:foo` with as well so I can
   specify `buck2 build //:foo --target-platforms=:<os>-asan-<link style>`
   to build `//:foo` with asan. It's impossible to specify "I just want
   asan on top of the defaults" with `--target-platforms`. Buckconfigs
   don't have this problem because they can compose on the command line
   via the `--config` flag.

4. *Target platforms are difficult to understand.*
   Most users can easily grasp how buckconfigs work because it's easy to
   set and read them. Very few understand how configurations work
   because of how complex target platform resolution is. Instead, it
   would be much simpler if constraints like OS and sanitizers can be
   set directly on the command line, similar to how buckconfigs are set.

This RFC proposes a new configuration API that is simpler and easier to
use than target platforms. The API is composition-first and avoids the
exponential permutation generation of platforms. The goal of this API is
to enable the migration of buckconfigs to configurations and make build
setting easy for users to use.

## API

A configuration is a collection of constraints.

In this API, every top-level target starts with an empty configuration.
To resolve the default target configuration, Buck will apply a list of
"modifiers". A modifier is a modification on the existing configuration
to obtain a new configuration.

A modifier can be applied on the command line, on a PACKAGE, or on a
target. When resolving modifiers, Buck will collect all modifiers from
command line, PACKAGE, and target and apply them in order until the
target configuration is obtained.

A modifier can be specified as a `constraint_value`, a `config_setting`,
`platform`, or a `cfg_override` target. If the modifier applied
is a constraint value, that constraint value will be added to the
configuration. Config setting and platform are both collections of
constraints, so if the modifier is a config setting or a platform, then
we loop over every constraint value and add it to the configuration
(note that since modifiers only work with configuration and not
buckconfigs, using a `config_setting` with a non-empty set of
buckconfigs as a modifier will be an error).
`cfg_override` rule applies a transition-like function
that can arbitrarily insert or delete constraints
from the existing configuration.

### 1. Required Modifiers

Every top-level target starts with an empty configuration.
Buck first applies "required modifiers", which is a list of modifiers
provided by the user via the command line.
The required modifiers are specified as
`buck2 build <target>?<modifiers separated by commas>`,
and the list of modifiers will be applied sequentially in that order.

For example,

- If the user requests `buck2 build //foo:bar`, `repo//foo:bar` will
  have an empty configuration after resolving the required modifiers.
- If the user requests
  `buck2 build repo//foo:bar?config//build_mode/constraints:nosan`,
  `repo//foo:bar` will have `config//build_mode/constraints:nosan`
  in its configuration after resolving required modifiers.
- `buck2 build repo//foo:bar?config//build_mode/constraints:nosan,config//build_mode/constraints:split-dwarf`
  will add nosan and split-dwarf in that order to the empty
  configuration.
- If the user requests
  `buck2 build repo//foo:bar?config//build_mode/constraints:nosan,config//build_mode/constraints:asan`
  where both constraint values belong to the same constraint
  `config//build_mode/constraints:san`,
  then asan will replace nosan in `repo//foo:bar`'s configuration.

To make constraints more convenient to type, you can use Buck's
target alias feature to create an alias for a constraint.
For example, you can add the following line to your .buckconfig

```ini
[alias]
  asan = config//build_mode/constraints:asan
  nosan = config//build_mode/constraints:nosan
```

to use `buck2 build repo//foo:bar?asan` and `buck2 build repo//foo:bar?nosan` as shorthand.

`buck2 build repo//foo/...?asan` and `buck2 build repo//foo:?asan`
are both valid.

To specify modifiers to a list of target patterns on the command line,
you can use the `--cfg=modifier` flag.
For example, `buck2 build //foo:bar //foo:baz --cfg=asan`
is equivalent to `buck2 build repo//foo:bar?asan //foo:baz?asan`.

`--cfg` flag can be specified multiple times to add multiple modifiers.
For example, `buck2 build --cfg=release --cfg=windows repo//foo:bar`
is equivalent to `buck2 build repo//foo:bar?release,windows`.
This behavior is consistent with how flags are interpreted
in other tools, and it is the most intuitive behavior, for example,
* buckconfig `-c` flag adds to the one set of buckconfigs
* build systems like CMake have flags like `-D` that can be specified
  multiple times, and they are all added to one set of flags.

It is prohibited to specify both `--cfg` flag and `?` in target pattern.
`--cfg` flag exists for convenience, and to specify
complex configuration setups (in scripts or in CI),
users can always specify `?`.
This restriction can be lifted in the future if there is a need.

When specifying a subtarget and modifier with `?`,
subtarget should go before modifier,
ex. `buck2 build repo//foo:bar[comp-db]?asan`.

The configuration after all required modifiers are resolved is known as
required configuration, and all constraints within that configuration
are immutable entries of the target configuration. They cannot be
removed or overwritten. This guarantees that the configuration a target
ends up with always include the modifiers user requested.

For example, if the user requests asan on the command line, the target
configuration should always end up with an asan constraint value, not a
nosan constraint value. If a later PACKAGE or per-target modifier
specifies nosan as a constraint, nosan will not be added to the
configuration because asan and nosan both belong to the same
constraint `config//build_mode/constraints:sanitizer`, and asan is a
required constraint.

### 2. (Legacy) Target platform

If a target platform is specified on a target
(either via the `default_target_platform` attribute
or the `--target-platforms` flag), then that target platform
gets applied as a modifier, where Buck loops over every constraint
within the platform and add it to the target configuration
unless that constraint conflicts with the required configuration.

When required, PACKAGE, and target modifiers are empty,
the target configuration is the target platform.
This is designed to making the new API interop
with target platform resolution so that the migration
can be gradual.

### 3. Per-PACKAGE modifiers

Per-PACKAGE modifiers are applied after target platform is resolved.

To define a per-PACKAGE modifier, you can use the
`add_default_cfg(<modifier>)` function in a PACKAGE file.
A modifier set through `add_default_cfg` will apply to
all targets covered by that PACKAGE.

The following example adds nosan and debug to the default target
configurations of all targets in `repo//foo/...` unless the required
configuration already has conflicting required constraints.

```python
# foo/PACKAGE

add_default_cfg("config//build_mode/constraints:nosan")
add_default_cfg("config//build_mode/constraints:debug")
```

In this example, debug is applied after nosan.

If a sub-PACKAGE exists for a PACKAGE, that sub-PACKAGE
will always inherit modifiers from its parent.
This means that if there are multiple PACKAGE files in the path
of a target, we apply their modifiers in order from the topmost PACKAGE.

Suppose `repo/PACKAGE` and `repo/foo/PACKAGE` exist for
target `repo//foo:bar`. In this case, modifiers in repo/PACKAGE apply
first before modifiers in `repo/foo/PACKAGE`. PACKAGE inheritance of
modifiers means that project-level modifiers can be easily set for all
targets under a project without having to wire logic through the bzl
wrapper of every rule type in that project.

### 4. Per-Target Modifier

Per-target modifier are applied after all per-PACKAGE modifier
are resolved.

Modifiers can be set on any target through the `default_cfg` attribute.
The modifiers are specified as a list and will be applied
in sequential order.

For example,
```python
# repo/foo/TARGETS

python_binary(
  name = "bar",
  # Some other attributes...
  default_cfg = [
    "config//build_mode/constraints:nosan",
    "config//build_mode/constraints:debug",
  ],
)
```
adds nosan and debug constraints in that order to
`repo//foo:bar`'s configuration.

Once per-target modifiers are resolved, we end up
with a target configuration used to configure a target.

Note that that target's configuration can further change after modifiers
are resolved if there are per-rule transitions applied on the target.

### `cfg_override` rule

A modifier can be defined as a `cfg_override` rule in addition
to platform and constraint value. This rule applies a function
that takes in the existing configuration as input, changes it,
and returns a new configuration.

This allows more complex logic like adding a constraint only if
an existing constraint exists.

The following example shows a way to express "use msvc on windows
configuration but clang on other OSes".

```python
# repo/BUCK

def _msvc_if_windows_else_clang_impl(
  # `Configuration` is an opaque object that supports methods `contains`, `get`, `insert`, and `pop` similar to a dictionary.
  cfg: "Configuration",
  # `Refs` holds references to dependent constraints, platforms, and modifiers.
  refs,
) -> "Configuration":
  if cfg.contains(refs.windows):
    cfg.insert(refs.msvc)
  else:
    cfg.insert(refs.clang)
  return cfg

cfg_override(
  name = "msvc_if_windows_else_clang",
  impl = _msvc_if_windows_else_clang_impl,
  refs = {
    "clang": "config//compiler/constraints:clang",
    "msvc": "config//compiler/constraints:msvc",
    "windows": "config//os/constraints:windows",
  },
)

# repo/PACKAGE
add_default_cfg(":msvc_if_windows_else_clang")
```

Now a target will always have the msvc constraint added
when targeting windows but the clang constraint otherwise.

It's possible to make `cfg_override` significantly less verbose
via a lambda. For example, the above example can be rewritten as
```python
# repo/BUCK

cfg_override(
  name = "msvc_if_windows_else_clang",
  # `cfg.set` returns a `Configuration` object.
  impl = lambda cfg, refs: cfg.set(
    refs.msvc if cfg.contains(refs.windows) else refs.clang
  ),
  refs = {
    "clang": "config//compiler/constraints:clang",
    "msvc": "config//compiler/constraints:msvc",
    "windows": "config//os/constraints:windows",
  },
)
```

`cfg_override` target can only be specified for PACKAGE
and per-target modifiers, not required modifiers
(this restriction can be revisited later if needed).

`cfg_override` is only intended for complex cases needed
to selectively insert constraints based on existing constraints.
For the majority of use cases, specifying constraints directly
as modifiers should be sufficient.

Like other types of modifiers, `cfg_override` cannot override
required constraints.
To guarantee this, `insert` and `pop` methods of `Configuration` object
will be no-ops if it is trying to overwrite a required constraint.

For example, in the previous example, if the user supplies a requested
configuration of windows and clang, then the target configuration
will end up containing windows and clang, not windows and msvc.

To make it clear which constraints are required
to the `cfg_override` function, the `Configuration` object
supports a `required_cfg` function that returns
a `RequiredConfiguration` object holding only the required constraints.
A `RequiredConfiguration` is immutable and supports
`contains` and `get` methods but not `insert`, `set`, or `pop`.

### Configuration Naming

It's important for a configuration to have a representative name
to indicate what important constraints were used.
This is useful for debugging because often a build error
is caused by a misconfiguration.

To make this easy, this API adds a naming function to derive a useful
name based on the existing configuration. The name function takes in a
map of constraint settings to constraint values as input and returns a
string for the name of the configuration. This name function will be
defined globally for a repo.

An example is as follows.
```python
# Generated configuration follows the name <os>-<sanitizer>-<toolchain>.
KEY_CONSTRAINT_SETTINGS = [
  "config//os/constraints:os",
  "config//build_mode/constraints:sanitizer",
  "config//compiler/constraints:toolchain",
]

def _name(
  # Keys and values in `constraints_map` are fully qualified target label strings.
  constraint_map,   # {str.type: str.type}
) -> str.type:
  name_builder = []
  for constraint_setting in KEY_CONSTRAINT_SETTINGS:
    constraint_value = constraint_map.get(constraint_setting)
    if constraint_value is not None:
      # Get the target name from full target label and add it to the generated name.
      name_builder.append(constraint_value.split(":")[-1])

  return "-".join(name_builder)
```

## Debugging modifiers

Because many layers of modifiers can be applied before obtaining
a final configuration, it is important that modifiers are easy
to debug for integrators. To show what modifiers are applied,
we can add a `buck.modifiers` special attribute to every target
that keeps track of all the required modifiers, legacy target platform,
per-PACKAGE modifiers and per-target modifiers a target goes through
in order. We can also add a `buck2 audit cfg-modifiers <TARGET>`
command to show the configuration change after each modifier
is applied as well as which PACKAGE/BUCK/TARGETS file
each modifier is added.

## How configuration modifiers differ from transitions

Modifiers are largely inspired by configuration transitions,
and there are a high amount of similarities in particular between
the `cfg_override` rule and the transition rule.

The major differences are:

1. A transition can change the configuration of a target when depending
   on a target, but a modifier can only change the configuration of a
   top-level target. In other words, if you have target A that depends
   on target B and you request a build of A, then A's target
   configuration would be resolved via modifiers and propagated down to
   B, but dep B would not do its own modifier resolution
2. `cfg_override` functions see an opaque "Configuration" object, so it
   cannot know every single constraint used in the configuration.
   Transition functions can iterate and read every constraint in the
   configuration. Transitions can use logic like "throw away the old
   configuration entirely and use a new configuration" (which is what
   fat platform transition currently does) whereas an override cannot.
3. Transitions can accept an `attrs` parameter from the attributes
   of the target if necessary whereas `cfg_override` does not
   (if necessary, this can be revisited).
4. `cfg_override` is specified as a target and transition is not.

Ideally, we should unify all the API differences between `cfg_override`
and `transition` and use `transition` directly instead,
but that's out of the scope of this RFC for now.

They have different use cases. For example,

1. *Python version* should be modeled as a transition and not modifier.
   Suppose we have `python_binary` A nested as a resource of
   another `python_binary` B. A should not inherit the python version
   from B, so a transition is needed to change A's python version
   when depended on by B.
2. *Library target* should use modifiers and not transitions.
   A C++ library target should always inherit the configuration
   of its parent C++ binary when it is used as a dep,
   but a top-level C++ library target can still have its configuration
   changed via modifiers when requested from command line.

## End Goal

1. No more `default_target_platform`.

2. No more `--target-platforms` flag.

3. There shouldn't be a need to define `platform` targets
   outside of exec platforms.

4. Most use cases of `read_config` are killed.
   Buckconfigs should be reserved for buck2 core features.

5. Most build settings should only use configurations.
