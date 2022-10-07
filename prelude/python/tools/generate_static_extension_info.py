#!/usr/bin/env python3

import argparse
import sys


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=argparse.FileType("w"), default=sys.stdout)
    parser.add_argument("--extension", action="append", default=[])
    args = parser.parse_args(argv[1:])
    out_file = args.output

    externs = []
    table = [
        "struct extension_info _static_extension_info[] = {",
    ]
    for python_name in args.extension:
        if python_name == "_static_extension_utils":
            continue
        # If this is a top level module we do not suffix the PyInit_ symbol
        index = python_name.rfind(".")
        if index > 0:
            module_name = python_name[index + 1 :]
            pyinit_suffix = python_name.replace(".", "_")
            pyinit_func = f"PyInit_{module_name}_{pyinit_suffix}"
        else:
            pyinit_func = f"PyInit_{python_name}"
        externs.append(f"PyMODINIT_FUNC {pyinit_func}(void);")
        table.append(f'  {{ "{python_name}", {pyinit_func} }},')
    table.append("  { NULL, NULL },")
    table.append("};")

    out_lines = (
        [
            '#include "Python.h"',
            "typedef PyObject* (*pyinitfunc)();",
            "struct extension_info {",
            "  const char *modname;",
            "  pyinitfunc initfunc;",
            "};",
        ]
        + externs
        + table
    )

    for line in out_lines:
        print(line, file=out_file)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
