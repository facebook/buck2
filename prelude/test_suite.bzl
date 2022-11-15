def test_suite_impl(_ctx: "context") -> ["provider"]:
    # There is nothing to implement here: test_suite exists as a mechanism to "group" tests using
    # the `tests` attribute, and the `tests` attribute is supported for all rules.
    return [DefaultInfo()]
