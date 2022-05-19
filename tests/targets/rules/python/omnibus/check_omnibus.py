import argparse
import os
import subprocess
import sys
import tempfile


def _expect(check, msg, *args, **kwargs):
    if not check:
        raise Exception("failed check: " + msg.format(*args, **kwargs))


def _path(root, lib):
    return os.path.join(root, lib)


def _deps(path):
    deps = set()
    out = subprocess.check_output(["objdump", "-p", path])
    for line in out.decode("utf-8").splitlines():
        parts = line.split()
        if len(parts) == 2 and parts[0] == "NEEDED":
            deps.add(parts[1])
    return deps


def _syms(path, defined=True):
    syms = set()
    cmd = ["nm", "-gPD", "--defined-only" if defined else "-u", path]
    out = subprocess.check_output(cmd)
    for line in out.decode("utf-8").splitlines():
        syms.add(line.split()[0])
    return syms


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--check-lib", action="append", default=[])
    parser.add_argument("--check-no-lib", action="append", default=[])
    parser.add_argument("--check-dep", action="append", default=[])
    parser.add_argument("--check-no-dep", action="append", default=[])
    parser.add_argument("--check-defined", action="append", default=[])
    parser.add_argument("--check-not-defined", action="append", default=[])
    parser.add_argument("--check-undefined", action="append", default=[])
    parser.add_argument("binary")
    args = parser.parse_args(argv[1:])

    binary = os.path.normpath(args.binary)
    link_tree = binary.replace(".par", "#link-tree")

    with tempfile.TemporaryDirectory() as tmp:
        if os.path.exists(link_tree):
            root = link_tree
        else:
            subprocess.check_call(["unzip", binary, "-d", tmp])
            root = tmp

        for lib in args.check_lib:
            _expect(os.path.exists(_path(root, lib)), "{} exists", lib)

        for lib in args.check_no_lib:
            _expect(not os.path.exists(_path(root, lib)), "{} exists", lib)

        for check in args.check_dep:
            lib, dep = check.split(":", 1)
            _expect(dep in _deps(_path(root, lib)), "{} depends on {}", lib, dep)

        for check in args.check_no_dep:
            lib, dep = check.split(":", 1)
            _expect(
                dep not in _deps(_path(root, lib)), "{} does not depend on {}", lib, dep
            )

        for check in args.check_defined:
            lib, sym = check.split(":", 1)
            _expect(
                sym in _syms(_path(root, lib), defined=True),
                "{} should define symbol {}",
                lib,
                sym,
            )

        for check in args.check_not_defined:
            lib, sym = check.split(":", 1)
            _expect(
                sym not in _syms(_path(root, lib), defined=True),
                "{} should not define symbol {}",
                lib,
                sym,
            )

        for check in args.check_undefined:
            lib, sym = check.split(":", 1)
            _expect(
                sym in _syms(_path(root, lib), defined=False),
                "{} should require symbol {}",
                lib,
                sym,
            )


sys.exit(main(sys.argv))
