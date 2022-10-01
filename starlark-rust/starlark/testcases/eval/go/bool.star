# @generated
# Copied from https://github.com/google/starlark-go/blob/e81fc95f7bd5bb1495fe69f27c1a99fcc77caa48/starlark/testdata/

# Tests of Starlark 'bool'

load("assert.star", "assert")

# truth
assert.true(True)
assert.true(not False)
assert.true(not not True)
assert.true(not not 1 >= 1)

# precedence of not
assert.true(not not 2 > 1)
# assert.true(not (not 2) > 1)   # TODO(adonovan): fix: gives error for False > 1.
# assert.true(not ((not 2) > 1)) # TODO(adonovan): fix
# assert.true(not ((not (not 2)) > 1)) # TODO(adonovan): fix
# assert.true(not not not (2 > 1))

# bool conversion
assert.eq(
    [bool(), bool(1), bool(0), bool("hello"), bool("")],
    [False, True, False, True, False],
)

# comparison
assert.true(None == None)
assert.true(None != False)
assert.true(None != True)
assert.eq(1 == 1, True)
assert.eq(1 == 2, False)
assert.true(False == False)
assert.true(True == True)

# ordered comparison
assert.true(False < True)
assert.true(False <= True)
assert.true(False <= False)
assert.true(True > False)
assert.true(True >= False)
assert.true(True >= True)

# conditional expression
assert.eq(1 if 3 > 2 else 0, 1)
assert.eq(1 if "foo" else 0, 1)
assert.eq(1 if "" else 0, 0)

# short-circuit evaluation of 'and' and 'or':
# 'or' yields the first true operand, or the last if all are false.
assert.eq(0 or "" or [] or 0, 0)
assert.eq(0 or "" or [] or 123 or 1 // 0, 123)
assert.fails(lambda : 0 or "" or [] or 0 or 1 // 0, "division by zero")

# 'and' yields the first false operand, or the last if all are true.
assert.eq(1 and "a" and [1] and 123, 123)
assert.eq(1 and "a" and [1] and 0 and 1 // 0, 0)
assert.fails(lambda : 1 and "a" and [1] and 123 and 1 // 0, "division by zero")

# Built-ins that want a bool want an actual bool, not a truth value.
# See github.com/bazelbuild/starlark/issues/30
assert.eq(''.splitlines(True), [])
assert.fails(lambda: ''.splitlines(1), 'got int, want bool')
assert.fails(lambda: ''.splitlines("hello"), 'got string, want bool')
assert.fails(lambda: ''.splitlines(0.0), 'got float, want bool')
