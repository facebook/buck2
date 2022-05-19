load("@fbcode//buck2/prelude/utils:utils.bzl", "value_or")

def go_attr_pkg_name(ctx: "context") -> str.type:
    """
    Return the Go package name for the given context corresponing to a rule.
    """
    return value_or(ctx.attr.package_name, ctx.label.package)

def merge_pkgs(pkgss: [{str.type: "artifact"}]) -> {str.type: "artifact"}:
    """
    Merge mappings of packages into a single mapping, throwing an error on
    conflicts.
    """

    all_pkgs = {}

    for pkgs in pkgss:
        for name, path in pkgs.items():
            if name in pkgs and pkgs[name] != path:
                fail("conflict for package {!r}: {} and {}".format(name, path, all_pkgs[name]))
            all_pkgs[name] = path

    return all_pkgs
