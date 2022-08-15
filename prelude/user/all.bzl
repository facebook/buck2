load("@fbcode//buck2/prelude/apple/user:apple_resource_bundle.bzl", _apple_resource_bundle_spec = "registration_spec")
load("@fbcode//buck2/prelude/apple/user:apple_tools.bzl", _apple_tools_spec = "registration_spec")
load("@fbcode//buck2/prelude/apple/user:apple_watchos_bundle.bzl", _apple_watchos_bundle_spec = "registration_spec")
load("@fbcode//buck2/prelude/apple/user:resource_group_map.bzl", _resource_group_map_spec = "registration_spec")
load("@fbcode//buck2/prelude/apple/user:watchos_bundle.bzl", _watchos_bundle_spec = "registration_spec")
load("@fbcode//buck2/prelude/cxx/user:link_group_map.bzl", _link_group_map_spec = "registration_spec")
load(":extract_archive.bzl", _extract_archive_spec = "registration_spec")

_all_specs = [
    _extract_archive_spec,
    _apple_tools_spec,
    _watchos_bundle_spec,
    _apple_resource_bundle_spec,
    _link_group_map_spec,
    _resource_group_map_spec,
    _apple_watchos_bundle_spec,
]

rules = {s.name: rule(impl = s.impl, attrs = s.attrs, **{k: v for k, v in {"cfg": s.cfg}.items() if v != None}) for s in _all_specs}

# The rules are accessed by doing module.name, so we have to put them on the correct module.
load_symbols(rules)
