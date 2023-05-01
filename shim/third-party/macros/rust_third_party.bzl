# @nolint

# Get current target platform. Hard-coded for now. Needs to match up with
# platforms defined in 'reindeer.toml'.
def _get_plat():
    if host_info().os.is_windows:
        return "windows-gnu"
    elif host_info().os.is_macos:
        return "macos"
    else:
        return "linux-x86_64"

def extend(orig, new):
    if orig == None:
        ret = new
    elif new == None:
        ret = orig
    elif type(orig) == type({}):
        ret = orig.copy()
        ret.update(new)
    else:  # list
        ret = orig + new
    return ret

# Add platform-specific args to args for a given platform. This assumes there's some static configuration
# for target platform (_get_plat) which isn't very flexible. A better approach would be to construct
# srcs/deps/etc with `select` to conditionally configure each target, but that's out of scope for this.
def platform_attrs(platformname, platformattrs, attrs):
    for attr in (attrs | platformattrs.get(platformname, {})).keys():
        new = extend(attrs.get(attr), platformattrs.get(platformname, {}).get(attr))
        attrs[attr] = new
    return attrs

def _adjust_mapped_srcs(kwargs):
    # We would rather if reindeer used named targets for the generated srcs,
    # as that is much more Windows friendly and more efficient.
    # But Buck1 doesn't like named targets in mapped_srcs, so we can't do this upstream.
    if "mapped_srcs" in kwargs:
        srcs = kwargs["mapped_srcs"]
        for k, v in srcs.items():
            pre, equal, post = k.partition("=")
            if equal:
                srcs.pop(k)
                srcs[pre + "[" + post + "]"] = v

def _sanitize_env(kwargs):
    # On Windows an env with a newline in it is hard to escape on the command line.
    # Deal with that by removing the newlines, which are usually in descriptions
    # (where they don't matter).
    if "env" in kwargs:
        env = kwargs["env"]
        for k, v in env.items():
            env[k] = v.replace("\n", "")

def third_party_rust_library(
        name, platform = {},
        dlopen_enable = None,
        linkable_alias = None,
        **kwargs):
    _unused = (dlopen_enable, linkable_alias)  # @unused
    _adjust_mapped_srcs(kwargs)
    _sanitize_env(kwargs)
    native.rust_library(name = name, **platform_attrs(_get_plat(), platform, kwargs))

# `platform` is a map from a platform (defined in reindeer.toml) to the attributes
# specific to that platform.
def third_party_rust_binary(name, platform = {}, **kwargs):
    _adjust_mapped_srcs(kwargs)
    _sanitize_env(kwargs)
    native.rust_binary(name = name, **platform_attrs(_get_plat(), platform, kwargs))

def third_party_rust_cxx_library(name, **kwargs):
    native.cxx_library(name = name, **kwargs)

def third_party_rust_prebuilt_cxx_library(name, **kwargs):
    # FIXME: This should probably be a fixup.toml, but it currently can't be expressed.
    # The windows-sys crate does -lwindows to find windows. We pass libwindows.a on the command line,
    # which resolves the symbols, but the linker still needs to "find" windows, so we also put its
    # directory on the link options.
    if name.endswith("libwindows.a"):
        kwargs["exported_linker_flags"] = ["-Lshim/third-party/rust/" + kwargs["static_lib"].rpartition("/")[0]]

    native.prebuilt_cxx_library(name = name, **kwargs)
