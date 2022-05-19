load("@fbcode_macros//build_defs:native_rules.bzl", "buck_genrule")

def check_subtarget(name, target, subtarget, expected_out_filename, expected_out_tokens = None):
    """
    Generate & run a buck_genrule that checks whether a given subtarget output file:
        - is generated AND
        - has the expected file name AND
        - contains a set of expected strings (if expected_out_tokens is not None)

    Args:
        name: name to be used for the generated buck_genrule
        target: the target to run $(location) on
        subtarget: the target's subtarget
        expected_out_filename: the expected output filename
        expected_out_tokens: the expected strings to be contained in the output file
    """

    cmd = []

    # Check: all strings present in list 'expected_out_tokens' are present in the generated
    # output file
    expected_out_tokens = expected_out_tokens or []
    for token in expected_out_tokens:
        err_str = "[error] expected '{}' to be in output file".format(token)
        cmd.append(
            "grep -F '{}' $(location {}[{}]) || {{ echo {} 1>&2; exit 1; }}".format(
                token,
                target,
                subtarget,
                err_str,
            ),
        )

    # Check: output file name is equal to 'expected_out_filename'
    err_str = "[error] expected output filename to be: {}".format(expected_out_filename)
    cmd.append(
        "echo $(location {}[{}]) | grep -F '{}' || {{ echo {} 1>&2; exit 1; }}".format(
            target,
            subtarget,
            expected_out_filename,
            err_str,
        ),
    )

    cmd.append("touch $OUT")

    buck_genrule(
        name = name,
        out = "out.txt",
        cmd = " && ".join(cmd),
    )
