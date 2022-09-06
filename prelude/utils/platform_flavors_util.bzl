def by_platform(
        platform_flavors: [str.type],
        xs: [(str.type, "_a")]) -> ["_a"]:
    """
    Resolve platform-flavor-specific parameters, given the list of platform
    flavors to match against.  Meant to mirror the usage of
    `PatternMatchedCollection`s in v1 for `platform_*` parameters.
    """

    res = []

    for (dtype, deps) in xs:
        for platform in platform_flavors:
            if regex_match(dtype, platform):
                res.append(deps)

    return res
