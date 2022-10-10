# @nolint

# Get current target platform - hard-coded for example, matches one of the platforms
# defined in reindeer.toml.
def _get_plat():
    if host_info().os.is_windows:
        return "windows"
    else:
        return "linux-x86_64"

# Matching host triple
def _get_native_host_triple():
    if host_info().os.is_windows:
        return "x86_64-pc-windows-msvc"
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
def _make_preamble(out_dir, package_name, version, features, cfgs, env, target_override):
    # Work out what rustc to pass to the script
    rustc = native.read_config("rust", "compiler", "rustc")
    if "//" in rustc:
        rustc = "(exe %s)" % rustc

    # CWD of a genrule script is the source directory but use $SRCDIR to make it an absolute path
    return """
        mkdir -p {out_dir}; \
        env \
            CARGO_MANIFEST_DIR=$SRCDIR/vendor/{package_name}-{version} \
            RUST_BACKTRACE=1 \
            OUT_DIR={out_dir} \
            CARGO=/bin/false \
            {features} \
            {cfgs} \
            CARGO_PKG_NAME={package_name} \
            CARGO_PKG_VERSION={version} \
            TARGET={target} \
            HOST={host} \
            RUSTC={rustc} \
            RUSTC_LINKER=/bin/false \
            `{rustc} --print cfg | awk -f $(location //third-party/macros:cargo_cfgs.awk)` \
            {env} \
    """.format(
        out_dir = out_dir,
        package_name = package_name,
        version = version,
        features = " ".join(["CARGO_FEATURE_{}=1".format(feature.upper().replace("-", "_")) for feature in features or []]),
        cfgs = " ".join(["CARGO_CFG_{}=1".format(cfg.upper().replace("-", "_")) for cfg in cfgs or []]),
        target = target_override or _get_native_host_triple(),
        host = _get_native_host_triple(),
        rustc = rustc,
        env = "\\\n".join(["'{}'='{}'".format(var, val) for var, val in (env or {}).items()]),
    )

# Invoke a Rust buildscript binary with the right surrounding
# environment variables.
def rust_buildscript_genrule_filter(name, buildscript_rule, outfile, package_name, version, features = None, cfgs = None, env = None, target = None):
    pre = _make_preamble("\\$(dirname $OUT)", package_name, version, features, cfgs, env, target)
    native.cxx_genrule(
        name = name,
        out = outfile,
        cmd = pre + "$(exe {buildscript}) > $OUT".format(
            buildscript = buildscript_rule,
        ),
    )

# Invoke a build script for its generated sources.
def rust_buildscript_genrule_srcs(name, buildscript_rule, files, package_name, version, features = None, cfgs = None, env = None, target = None, srcs = None):
    pre = _make_preamble("$OUT", package_name, version, features, cfgs, env, target)
    native.cxx_genrule(
        name = name,
        srcs = srcs,
        out = name + "-outputs",
        cmd = pre + "$(exe {buildscript})".format(
            buildscript = buildscript_rule,
        ),
    )
    mainrule = ":" + name
    for file in files:
        native.cxx_genrule(
            name = "{}={}".format(name, file),
            out = file,
            cmd = "mkdir -p \\$(dirname $OUT) && cp $(location {main})/{file} $OUT".format(
                main = mainrule,
                file = file,
            ),
        )

# Add platform-specific args to args for a given platform. This assumes there's some static configuration
# for target platform (_get_plat) which isn't very flexible. A better approach would be to construct
# srcs/deps/etc with `select` to conditionally configure each target, but that's out of scope for this.
def platform_attrs(platformname, platformattrs, attrs):
    for attr in (attrs | platformattrs.get(platformname, {})).keys():
        new = extend(attrs.get(attr), platformattrs.get(platformname, {}).get(attr))
        attrs[attr] = new
    return attrs

def third_party_rust_library(name, platform = {}, **kwargs):
    native.rust_library(name = name, **platform_attrs(_get_plat(), platform, kwargs))

# `platform` is a map from a platform (defined in reindeer.toml) to the attributes
# specific to that platform.
def third_party_rust_binary(name, platform = {}, **kwargs):
    native.rust_binary(name = name, **platform_attrs(_get_plat(), platform, kwargs))

def third_party_rust_cxx_library(name, **kwargs):
    native.cxx_library(name = name, **kwargs)

def third_party_rust_prebuilt_cxx_library(name, **kwargs):
    native.prebuilt_cxx_library(name = name, **kwargs)
