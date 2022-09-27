def _impl(_ctx):
    return [DefaultInfo()]

noop_rule = rule(impl = _impl, attrs = {})

def targets(n):
    for i in range(n):
        noop_rule(
            name = "{}".format(i),
        )
