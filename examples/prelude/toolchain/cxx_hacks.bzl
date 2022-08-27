def _cxx_hacks_impl(_ctx):
    return [DefaultInfo(), TemplatePlaceholderInfo(
        unkeyed_variables = {
            "cxx-header-tree": "/dev/null/HACK-CXX-HEADER-TREE",
            "output-dwo-dir": "/dev/null/HACK-OUTPUT-DWO-DIR",
        },
    )]

cxx_hacks = rule(
    impl = _cxx_hacks_impl,
    attrs = {},
)