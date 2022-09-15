def _rule_impl(_ctx):
    return [DefaultInfo()]

rule1 = rule(impl = _rule_impl, attrs = {"foo": attrs.string()})
rule2 = rule(impl = _rule_impl, attrs = {"foo": attrs.string()})
rule3 = rule(impl = _rule_impl, attrs = {"foo": attrs.string()})
