# @generated
# Copied from https://github.com/google/starlark-go/blob/e81fc95f7bd5bb1495fe69f27c1a99fcc77caa48/starlark/testdata/

# Miscellaneous tests of Starlark evaluation.
# This is a "chunked" file: each "---" effectively starts a new file.

# TODO(adonovan): move these tests into more appropriate files.
# TODO(adonovan): test coverage:
# - stmts: pass; if cond fail; += and failures;
#    for x fail; for x not iterable; for can't assign; for
#    error in loop body
# - subassign fail
# - x[i]=x fail in both operands; frozen x; list index not int; boundscheck
# - x.f = ...
# - failure in list expr [...]; tuple expr; dict expr (bad key)
# - cond expr semantics; failures
# - x[i] failures in both args; dict and iterator key and range checks;
#   unhandled operand types
# - +: list/list, int/int, string/string, tuple+tuple, dict/dict;
# - * and ** calls: various errors
# - call of non-function
# - slice x[ijk]
# - comprehension: unhashable dict key;
#   scope of vars (local and toplevel); noniterable for clause
# - unknown unary op
# - ordering of values
# - freeze, transitivity of its effect.
# - add an application-defined type to the environment so we can test it.
# - even more:
#
# eval
#   pass statement
#   assign to tuple l-value -- illegal
#   assign to list l-value -- illegal
#   assign to field
#   tuple + tuple
#   call with *args, **kwargs
#   slice with step
#   tuple slice
#   interpolate with %c, %%

load("assert.star", "assert")

# Ordered comparisons require values of the same type.
assert.fails(lambda: None < None, "not impl")
assert.fails(lambda: None < False, "not impl")
assert.fails(lambda: False < list, "not impl")
assert.fails(lambda: list < {}, "not impl")
assert.fails(lambda: {} < (lambda: None), "not impl")
assert.fails(lambda: (lambda: None) < 0, "not impl")
assert.fails(lambda: 0 < [], "not impl")
assert.fails(lambda: [] < "", "not impl")
assert.fails(lambda: "" < (), "not impl")
# Except int < float:
assert.lt(1, 2.0)
assert.lt(2.0, 3)

---
# cyclic data structures
load("assert.star", "assert")

cyclic = [1, 2, 3] # list cycle
cyclic[1] = cyclic
assert.eq(str(cyclic), "[1, [...], 3]")
assert.fails(lambda: cyclic < cyclic, "maximum recursion")
assert.fails(lambda: cyclic == cyclic, "maximum recursion")
cyclic2 = [1, 2, 3]
cyclic2[1] = cyclic2
assert.fails(lambda: cyclic2 == cyclic, "maximum recursion")

cyclic3 = [1, [2, 3]] # list-list cycle
cyclic3[1][0] = cyclic3
assert.eq(str(cyclic3), "[1, [[...], 3]]")
cyclic4 = {"x": 1}
cyclic4["x"] = cyclic4
assert.eq(str(cyclic4), "{\"x\": {...}}")
cyclic5 = [0, {"x": 1}] # list-dict cycle
cyclic5[1]["x"] = cyclic5
assert.eq(str(cyclic5), "[0, {\"x\": [...]}]")
assert.eq(str(cyclic5), "[0, {\"x\": [...]}]")
assert.fails(lambda: cyclic5 == cyclic5 ,"maximum recursion")
cyclic6 = [0, {"x": 1}]
cyclic6[1]["x"] = cyclic6
assert.fails(lambda: cyclic5 == cyclic6, "maximum recursion")

---
# regression
load("assert.star", "assert")

# was a parse error:
assert.eq(("ababab"[2:]).replace("b", "c"), "acac")
assert.eq("ababab"[2:].replace("b", "c"), "acac")

# test parsing of line continuation, at toplevel and in expression.
three = 1 + \
  2
assert.eq(1 + \
  2, three)

---
# A regression test for error position information.

_ = {}.get(1, default=2) ### "get: unexpected keyword arguments"

---
# Load exposes explicitly declared globals from other modules.
load('assert.star', 'assert', 'freeze')
assert.eq(str(freeze), '<built-in function freeze>')

---
# Load does not expose pre-declared globals from other modules.
# See github.com/google/skylark/issues/75.
load('assert.star', 'assert', 'matches') ### "matches not found in module"

---
# Load does not expose universals accessible in other modules.
load('assert.star', 'len') ### "len not found in module"


---
# Test plus folding optimization.
load('assert.star', 'assert')

s = "s"
l = [4]
t = (4,)

assert.eq("a" + "b" + "c", "abc")
assert.eq("a" + "b" + s + "c", "absc")
assert.eq(() + (1,) + (2, 3), (1, 2, 3))
assert.eq(() + (1,) + t + (2, 3), (1, 4, 2, 3))
assert.eq([] + [1] + [2, 3], [1, 2, 3])
assert.eq([] + [1] + l + [2, 3], [1, 4, 2, 3])

assert.fails(lambda: "a" + "b" + 1 + "c", "unknown binary op: string \\+ int")
assert.fails(lambda: () + () + 1 + (), "unknown binary op: tuple \\+ int")
assert.fails(lambda: [] + [] + 1 + [], "unknown binary op: list \\+ int")



---
load('assert.star', 'froze') ### `name froze not found .*did you mean freeze`
