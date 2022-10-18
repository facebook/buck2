def _impl(ctx):
    # @lint-ignore BUILDIFIERLINT
    _ignore = ctx
    return [DefaultInfo()]

# This bzl file cannot be interpreted with Buck1 because there's no `rule` builtin.
my_rule = rule(impl = _impl, attrs = {})
