prelude = native

def check_platform(name, target_platform, version):
    """
    Check that an extension built for the given target platform uses the given
    python version.
    """

    prelude.genrule(
        name = name,
        cmd = "$(exe :{}-bin) | grep '^{}$' > $OUT".format(name, version),
        out = "out.txt",
    )

    prelude.python_binary(
        name = name + "-bin",
        main_module = "buck2.tests.targets.rules.python.versions.ext_test",
        default_target_platform = target_platform,
        deps = [
            ":lib",
        ],
    )
