"""
This is the summary for the module

And these are its details
"""

def bar(a, b: "string", *, c: "string", d: "string" = "some_default") -> ["string"]:
    """
    This is the summary line for 'bar'

    These are the details that go below.
    We'll query for this symbol, and verify it matches json as expected
    Don't document 'd'

    Args:
        a: Docs for a
        b: Docs for b
        c: Docs for c
    """
    return ["a={a},  b={b},  c={c},  d={d}".format(a = a, b = b, c = c, d = d)]

def baz():
    """ Simple docstring for baz """
    pass

def quz():
    # No docstring
    pass

undocumented_variable = 5
