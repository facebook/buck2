import sys

# Verify that output contain expected lines
with open(sys.argv[1], "r") as out_file:
    """verify output of java_binary() and prints all lines from output"""
    lines = out_file.readlines()
    print(lines)

    expected_lines = set(sys.argv[2:])
    lines_to_find = len(expected_lines)
    found = 0
    for line in lines:
        if line.rstrip() in expected_lines:
            found += 1

    if found < lines_to_find:
        raise Exception(
            "Expected to find `{}` lines but found {}.\n\t\tLines: {}.\n\t\tExpected lines {}".format(
                lines_to_find, found, lines, expected_lines
            )
        )
