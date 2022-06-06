# `native` is fine to use in the prelude for v2
# @lint-ignore-every BUCKLINT

# This is buck2's shim import. Any public symbols here will be available within
# **all** interpreted files.

load("@fbcode//buck2/prelude:paths.bzl", "paths")
load("@fbcode//buck2/prelude/apple:apple_bundle_macro_layer.bzl", "apple_bundle_macro_impl")
load("@fbcode//buck2/prelude/cxx:cxx_toolchain_types.bzl", _cxx = "cxx")
load("@fbcode//buck2/prelude/python:toolchain.bzl", _python = "python")
load("@fbcode//buck2/prelude/user:all.bzl", _user_rules = "rules")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")
load(":rules.bzl", __rules__ = "rules")

def __struct_to_dict(s):
    vals = {}
    for name in dir(s):
        # this is a bug with starlark-rust that struct-type methods are listed in dir
        if name == "to_json":
            continue
        vals[name] = getattr(s, name)
    return vals

def _tp2_constraint(project, version):
    """
    Return the target configuration constraint to use for the given project
    version
    """

    return "ovr_config//third-party/{}/constraints:{}".format(project, version)

def _tp2_constraint_multi(versions):
    """
    Return the `select` key rule name which corresponds to the given tp2 project
    versions.
    """

    expect(len(versions) >= 1, str(versions))

    # If there's only a single project/version pair, then just return the its
    # pre-defined constraint value rule name.
    if len(versions) == 1:
        (project, version) = versions.items()[0]
        return _tp2_constraint(project, version)

    # Otherwise, generate a `config_setting` to combine the constraint value
    # rules for all the project/version pairs.
    name = "-".join(["_tp2_constraints_"] + ["{}-{}".format(p, v) for p, v in sorted(versions.items())])
    if not native.rule_exists(name):
        native.config_setting(
            name = name,
            constraint_values = [_tp2_constraint(p, v) for p, v in versions.items()],
        )
    return ":" + name

def _extract_versions(constraints):
    """
    Convert v1-style version constraints to a v2-compatible config settings.

    The constraints are normally of the form:
    `{"//third-party-buck/platform*/build/python:__project__": "3.8"}`.
    """

    versions = {}

    # Since the constraints will be duplicated for each fbcode "platform", do
    # some initial work to de-duplicate them here, by extracting just the
    # project and version and verify we get just a single reduced result.
    for target, version in constraints.items():
        expect(target.startswith("fbcode//") or target.startswith("//"), target)
        base, name = target.split(":")
        expect(name == "__project__", name)
        project = paths.basename(base)
        expect(project not in versions or version == versions[project])
        versions[project] = version

    return versions

def _versioned_param_to_select(items, default = None):
    """
    Convert a v1-style `versioned_*` param to a `select` map.

    Parameters:
    - items: A list of 2-tuples of a list of version constraints to match and
          the values to use in case of match.
    """

    if items == None:
        return None

    # Special case a form of "empty" constraints that `buckify_tp2` may
    # generate in tp2 TARGETS.
    if len(items) == 1 and not items[0][0]:
        return items[0][1]

    select_map = {}

    # If a default is provided add that.
    if default != None:
        select_map["DEFAULT"] = default

    # Convert v1 tp2-style versioned_* params to their analogous v2 select
    # constraint maps.
    for constraints, item in items:
        versions = _extract_versions(constraints)
        select_map[_tp2_constraint_multi(versions)] = item

    if not select_map:
        return None

    return select(select_map)

def _concat(*items):
    """
    Concatenate non-`None` items and return result.
    """
    res = None

    for item in items:
        if item == None:
            continue
        if res == None:
            res = item
        elif type(res) == type({}) and type(item) == type({}):
            new_res = {}
            new_res.update(res)
            new_res.update(item)
            res = new_res
        else:
            res += item

    return res

def _at_most_one(*items):
    """
    Return a non-`None` value if it exists.  Fail if more that one non-`None`
    exists.
    """

    res = None

    for item in items:
        if item == None:
            continue
        expect(res == None)
        res = item

    return res

# export_file src defaults to name, despite being string vs source, so adjust it in the macros
def _export_file_macro_stub(name, src = None, **kwargs):
    __rules__["export_file"](name = name, src = name if src == None else src, **kwargs)

def _prebuilt_cxx_library_macro_stub(
        exported_preprocessor_flags = None,
        versioned_exported_preprocessor_flags = None,
        exported_lang_preprocessor_flags = None,
        versioned_exported_lang_preprocessor_flags = None,
        exported_platform_preprocessor_flags = None,
        versioned_exported_platform_preprocessor_flags = None,
        exported_lang_platform_preprocessor_flags = None,
        versioned_exported_lang_platform_preprocessor_flags = None,
        static_lib = None,
        versioned_static_lib = None,
        static_pic_lib = None,
        versioned_static_pic_lib = None,
        shared_lib = None,
        versioned_shared_lib = None,
        header_dirs = None,
        versioned_header_dirs = None,
        **kwargs):
    __rules__["prebuilt_cxx_library"](
        exported_preprocessor_flags = _concat(
            exported_preprocessor_flags,
            _versioned_param_to_select(versioned_exported_preprocessor_flags),
        ),
        exported_lang_preprocessor_flags = _concat(
            exported_lang_preprocessor_flags,
            _versioned_param_to_select(versioned_exported_lang_preprocessor_flags),
        ),
        exported_platform_preprocessor_flags = _concat(
            exported_platform_preprocessor_flags,
            _versioned_param_to_select(versioned_exported_platform_preprocessor_flags),
        ),
        exported_lang_platform_preprocessor_flags = _concat(
            exported_lang_platform_preprocessor_flags,
            _versioned_param_to_select(versioned_exported_lang_platform_preprocessor_flags),
        ),
        static_lib = _at_most_one(static_lib, _versioned_param_to_select(versioned_static_lib)),
        static_pic_lib = _at_most_one(static_pic_lib, _versioned_param_to_select(versioned_static_pic_lib)),
        shared_lib = _at_most_one(shared_lib, _versioned_param_to_select(versioned_shared_lib)),
        header_dirs = _at_most_one(header_dirs, _versioned_param_to_select(versioned_header_dirs)),
        **kwargs
    )

def _python_library_macro_stub(
        srcs = None,
        versioned_srcs = None,
        resources = None,
        versioned_resources = None,
        **kwargs):
    __rules__["python_library"](
        srcs = _concat(srcs, _versioned_param_to_select(versioned_srcs, default = {})),
        resources = _concat(resources, _versioned_param_to_select(versioned_resources, default = {})),
        **kwargs
    )

def _versioned_alias_macro_stub(versions = {}, **kwargs):
    project = paths.basename(native.package_name())
    __rules__["alias"](
        actual = select({
            _tp2_constraint(project, version): actual
            for version, actual in versions.items()
        }),
        **kwargs
    )

def _configured_alias_macro_stub(name, actual, platform, **kwargs):
    configured_alias_impl = __rules__["configured_alias"]

    # `actual` needs to be a pair of target + platform, as that's the format
    # expected by the `configured_dep()` field
    configured_alias_impl(name = name, configured_actual = (actual, platform), actual = actual, platform = platform, **kwargs)

def _apple_bundle_macro_stub(**kwargs):
    apple_bundle_macro_impl(
        apple_bundle_rule = __rules__["apple_bundle"],
        **kwargs
    )

# TODO(cjhopman): These macro wrappers should be handled in prelude/rules.bzl+rule_impl.bzl.
# Probably good if they were defined to take in the base rule that
# they are wrapping and return the wrapped one.
__extra_rules__ = {
    "apple_bundle": _apple_bundle_macro_stub,
    "configured_alias": _configured_alias_macro_stub,
    "export_file": _export_file_macro_stub,
    "prebuilt_cxx_library": _prebuilt_cxx_library_macro_stub,
    "python_library": _python_library_macro_stub,
    "versioned_alias": _versioned_alias_macro_stub,
}

__shimmed_native__ = __struct_to_dict(__internal__)
__shimmed_native__.update(__rules__)
__shimmed_native__.update(__extra_rules__)
__shimmed_native__.update({"cxx": _cxx, "python": _python})
__shimmed_native__.update(_user_rules)

# The `json` symbol isn't available in Buck1, so to use it conditionally
# we need to write `native.json` or it will be a name binding error.
__shimmed_native__.update({"json": json})

native = struct(**__shimmed_native__)
