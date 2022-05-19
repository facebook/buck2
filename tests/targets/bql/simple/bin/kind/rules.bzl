def _rule_impl(_ctx):
    return [DefaultInfo()]

rule1 = rule(implementation = _rule_impl, attrs = {"foo": attr.string()})
rule2 = rule(implementation = _rule_impl, attrs = {"foo": attr.string()})
rule3 = rule(implementation = _rule_impl, attrs = {"foo": attr.string()})
