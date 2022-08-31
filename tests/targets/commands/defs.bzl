prelude = native

def get_deps_based_on_config_value():
    if prelude.read_config("user", "deps_enabled", "false") != "false":
        return ["fbcode//buck2/tests/targets/commands:dynamic"]
    return []

def _impl(_ctx):
    pass

foo_library = rule(
    impl = _impl,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
    },
)

def define_targets_based_on_config_value():
    if prelude.read_config("user", "targets_enabled", "false") != "false":
        foo_library(
            name = "config_defined_target",
        )
