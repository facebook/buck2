def _rule_with_query(ctx):
    return [DefaultInfo()]

rule_with_query = rule(
    impl = _rule_with_query,
    attrs = {"query": attrs.query()},
)
