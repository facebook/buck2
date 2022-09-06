load(":tr.bzl", "transition_increase_label_len")

def _impl(ctx):
    _ = ctx
    fail("Don't care")

my_rule = rule(impl = _impl, attrs = {}, cfg = transition_increase_label_len)
