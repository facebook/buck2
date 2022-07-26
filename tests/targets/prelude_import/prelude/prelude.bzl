# @lint-ignore BUCKRESTRICTEDSYNTAX
config = read_config("test", "config")

def _check_config_impl(_ctx):
    # This checks that the config we read is the one that was read in the
    # prelude cell (our other cells have different configs).
    if config != "prelude":
        fail("Unexpected config!")
    return [DefaultInfo()]

check_config = rule(attrs = {}, impl = _check_config_impl)
