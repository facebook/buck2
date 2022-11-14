#!/usr/bin/env python3

import argparse
import sys


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=argparse.FileType("w"), default=sys.stdout)
    parser.add_argument("--entry-point", type=str, default="")
    parser.add_argument("--extension", action="append", default=[])
    args = parser.parse_args(argv[1:])
    out_file = args.output

    externs = []
    table = [
        "struct _inittab _static_extension_info[] = {",
    ]
    for python_name in args.extension:
        module_name, pyinit_func = python_name.split(":")
        # If this is a top level module we do not suffix the PyInit_ symbol
        externs.append(f"PyMODINIT_FUNC {pyinit_func}(void);")
        table.append(f'  {{ "{module_name}", {pyinit_func} }},')
    table.append("  { nullptr, nullptr },")
    table.append("};")

    out_lines = (
        [
            '#include "Python.h"',
            '#include "import.h"',
            f'extern const char _static_extension_entry_point[] = "{args.entry_point}";',
        ]
        + externs
        + table
    )

    for line in out_lines:
        print(line, file=out_file)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
