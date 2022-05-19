# Parse and resolve Makefile's produce by ocamldeps.
load("@fbcode//buck2/prelude:paths.bzl", "paths")

# Given the contents of a Makefile, and the sources we care about produce a
# mapping of which artifacts depend on which.
def parse_makefile(makefile: str.type, srcs: ["artifact"], bytecode: bool.type) -> {"artifact": ["artifact"]}:
    contents = _parse(makefile)
    if bytecode:
        contents = [_replace_cmx_with_cmo(c) for c in contents]

    # Collect all the files we refrence
    files = {}
    for aa, bb in contents:
        for x in aa + bb:
            files[x] = None

    resolved = _resolve(files.keys(), srcs)
    result = {}

    # This nested loop is technically O(n^2), but since `a` is at most 2 in OCaml
    # it's not an issue
    for aa, bb in contents:
        for a in aa:
            if a in resolved:
                result[resolved[a]] = [resolved[b] for b in bb if b in resolved]
    return result

def _resolve(entries: [str.type], srcs: ["artifact"]) -> {str.type: "artifact"}:
    # O(n^2), alas - switch to a prefix map if that turns out to be too slow.
    result = {}
    for x in entries:
        prefix, ext = paths.split_extension(x)
        if ext in (".cmo", ".cmx"):
            y = prefix + ".ml"
        elif ext == ".cmi":
            y = prefix + ".mli"
        else:
            fail("ocamldeps makefile, unexpected extension: " + x)
        possible = [src for src in srcs if y.endswith("/" + src.short_path)]
        if len(possible) == 1:
            result[x] = possible[0]
        elif len(possible) == 0:
            # If we get here, then ocamldep has found a dependency on a .ml file
            # that wasn't in `srcs`. On Remote Execution, that shouldn't happen,
            # and would be user error. When running locally, ocamldep will look
            # at any .ml files that are in the same directory as a `srcs` (no
            # way to stop it), so sometimes it finds those when they are meant
            # to be opaque or coming in through `deps`. The only solution is
            # just to ignore such unknown entries and "pretend" the .ml file
            # wasn't found.
            pass
        else:
            fail("ocamldeps makefile, multiple sources: " + x)
    return result

# Do the low level parse, returning the strings that are in the makefile as
# dependencies
def _parse(makefile: str.type) -> [([str.type], [str.type])]:
    # For the OCaml makefile, it's a series of lines, often with a '\' line
    # continuation.
    lines = []
    line = ""
    for x in makefile.splitlines():
        if x.endswith("\\"):
            line += x[:-1]
        else:
            lines.append(line + x)
            line = ""
    lines.append(line)

    result = []
    for x in lines:
        before, _, after = (x + " ").partition(" : ")
        result.append((before.split(), after.strip().split()))
    return result

# Change suffix ".cmx" to ".cmo" in the results of a parsed makefile.
def _replace_cmx_with_cmo(p: ([str.type], [str.type])) -> ([str.type], [str.type]):
    def cmx_to_cmo(p):
        pre, ext = paths.split_extension(p)
        if ext == ".cmx":
            return pre + ".cmo"
        return p

    xs, ys = p
    return ([cmx_to_cmo(x) for x in xs], [cmx_to_cmo(y) for y in ys])
