load(":tr.bzl", "iphone_to_watch_transition")

def _nop_op(*_args, **_kwargs):
    fail("this is cquery only test, no rules are executed")

my_little_iphone_binary = rule(_nop_op, attrs = {
    "watch_resource": attr.transition_dep(cfg = iphone_to_watch_transition),
})

my_resource = rule(_nop_op, attrs = {})

my_alias = rule(_nop_op, attrs = {
    "to": attr.dep(),
})
