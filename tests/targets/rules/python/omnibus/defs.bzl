prelude = native

def check_omnibus(
        name,
        binary,
        check_libs = [],
        check_no_libs = [],
        check_deps = [],
        check_no_deps = [],
        check_defined = [],
        check_not_defined = [],
        check_undefined = []):
    prelude.genrule(
        name = name,
        cmd = " && ".join([
            "$(exe {})".format(binary),
            " ".join(
                ["$(exe fbcode//buck2/tests/targets/rules/python/omnibus:check_omnibus)"] +
                ["--check-lib={}".format(lib) for lib in check_libs] +
                ["--check-no-lib={}".format(lib) for lib in check_no_libs] +
                ["--check-dep={}:{}".format(lib, d) for lib, d in check_deps] +
                ["--check-no-dep={}:{}".format(lib, d) for lib, d in check_no_deps] +
                ["--check-defined={}:{}".format(lib, s) for lib, s in check_defined] +
                ["--check-not-defined={}:{}".format(lib, s) for lib, s in check_not_defined] +
                ["--check-undefined={}:{}".format(lib, s) for lib, s in check_undefined] +
                ["$(location {})".format(binary)],
            ),
            "touch $OUT",
        ]),
        out = "out.txt",
    )
