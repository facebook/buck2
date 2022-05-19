def implementation(_ctx):
    return [DefaultInfo()]

noop_rule = rule(implementation = implementation, attrs = {})

def targets(n):
    for i in range(n):
        noop_rule(
            name = "{}".format(i),
        )
