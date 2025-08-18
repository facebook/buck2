def assert_equals(expected, actual, msg = None):
    if expected != actual:
        if msg == None:
            fail("expected: {}, got: {}".format(expected, actual))
        else:
            fail("{}: expected: {}, got: {}".format(msg, expected, actual))