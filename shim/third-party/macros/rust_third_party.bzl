# @nolint

# Get current target platform - hard-coded for example, matches one of the platforms
# defined in reindeer.toml.
def _get_plat():
    if host_info().os.is_windows:
        return "windows-gnu"
    else:
        return "linux-x86_64"

# Matching host triple
def _get_native_host_triple():
    if host_info().os.is_windows:
        return "x86_64-pc-windows-gnu"
    else:
        return "x86_64-unknown-linux-gnu"

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

# Invoke something with a default cargo-like environment. This is used to invoke buildscripts
# from within a Buck rule to get it to do whatever it does (typically, either emit command-line
# options for rustc, or generate some source).
def _make_cmd(mode, buildscript, package_name, version, features, cfgs, env, target_override):
    flags = [
        ("mode", mode),
        ("buildscript", "$(exe " + buildscript + ")"),
        ("package-name", package_name),
        ("version", version),
        ("feature", features),
        ("cfg", cfgs),
        ("env", env),
        ("target", target_override or _get_native_host_triple()),
    ]

    cmd = "$(exe shim//third-party/macros:build_rs)"
    # We don't want to quote the $OUT flag as it might end in \ on Windows, which would then escape the quote
    cmd += " --output=$OUT"
    for flag, value in flags:
        if value == None:
            pass
        elif type(value) == type([]):
            for x in value:
                cmd += " \"--" + flag + "=" + x + "\""
        elif type(value) == type({}):
            for k, v in value.items():
                cmd += " \"--" + flag + "=" + k + "=" + v + "\""
        else:
            cmd += " \"--" + flag + "=" + value + "\""
    return cmd


# Invoke a Rust buildscript binary with the right surrounding
# environment variables.
def rust_buildscript_genrule_filter(name, buildscript_rule, outfile, package_name, version, features = None, cfgs = None, env = None, target = None):
    cmd = _make_cmd("args", buildscript_rule, package_name, version, features, cfgs, env, target)
    native.genrule(
        name = name,
        out = outfile,
        cmd = cmd,
    )

# Invoke a build script for its generated sources.
def rust_buildscript_genrule_srcs(name, buildscript_rule, files, package_name, version, features = None, cfgs = None, env = None, target = None, srcs = None):
    pre = _make_cmd("srcs", buildscript_rule, package_name, version, features, cfgs, env, target)
    native.cxx_genrule(
        name = name,
        srcs = srcs,
        outs = {file: [file] for file in files},
        cmd = pre,
    )

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
    # Deal with that by removing the newlines, which are usually in descrptions
    # (where they don't matter).
    if "env" in kwargs:
        env = kwargs["env"]
        for k, v in env.items():
            env[k] = v.replace("\n", "")

def third_party_rust_library(name, platform = {}, **kwargs):
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
