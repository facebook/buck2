load("@fbcode_macros//build_defs:native_rules.bzl", "buck_genrule")

def check_omnibus(
        name,
        binary,
        check_libs = [],
        check_no_libs = [],
        check_deps = [],
        check_no_deps = [],
        check_defined = [],
        check_not_defined = [],
        check_undefined = [],
        check_symlink = []):
    ext = "dylib" if native.host_info().os.is_macos else "so"
    buck_genrule(
        name = name,
        cmd = " && ".join([
            "$(exe {})".format(binary),
            " ".join(
                ["$(exe fbcode//buck2/tests/targets/rules/python/omnibus:check_omnibus)"] +
                ["--check-lib={}".format(lib.format(ext = ext)) for lib in check_libs] +
                ["--check-no-lib={}".format(lib.format(ext = ext)) for lib in check_no_libs] +
                ["--check-dep={}:{}".format(lib.format(ext = ext), d.format(ext = ext)) for lib, d in check_deps] +
                ["--check-no-dep={}:{}".format(lib.format(ext = ext), d.format(ext = ext)) for lib, d in check_no_deps] +
                ["--check-defined={}:{}".format(lib.format(ext = ext), s) for lib, s in check_defined] +
                ["--check-not-defined={}:{}".format(lib.format(ext = ext), s) for lib, s in check_not_defined] +
                ["--check-undefined={}:{}".format(lib.format(ext = ext), s) for lib, s in check_undefined] +
                ["--check-symlink={}:{}".format(lib.format(ext = ext), s) for lib, s in check_symlink] +
                ["$(location {})".format(binary)],
            ),
            "touch $OUT",
        ]),
        out = "out.txt",
    )
