# @generated
# Copied from https://github.com/google/starlark-go/blob/e81fc95f7bd5bb1495fe69f27c1a99fcc77caa48/starlark/testdata/

# Tests of Starlark assignment.

# This is a "chunked" file: each "---" effectively starts a new file.

# tuple assignment
load("assert.star", "assert")

() = () # empty ok

a, b, c = 1, 2, 3
assert.eq(a, 1)
assert.eq(b, 2)
assert.eq(c, 3)

(d, e, f,) = (1, 2, 3) # trailing comma ok
---
(a, b, c) = 1 ### "got int in sequence assignment"
---
(a, b) = () ### "too few values to unpack"
---
(a, b) = (1,) ### "too few values to unpack"
---
(a, b, c) = (1, 2) ### "too few values to unpack"
---
(a, b) = (1, 2, 3) ### "too many values to unpack"
---
() = 1 ### "got int in sequence assignment"
---
() = (1,) ### "too many values to unpack"
---
() = (1, 2) ### "too many values to unpack"
---
# list assignment
load("assert.star", "assert")

[] = [] # empty ok

[a, b, c] = [1, 2, 3]
assert.eq(a, 1)
assert.eq(b, 2)
assert.eq(c, 3)

[d, e, f,] = [1, 2, 3] # trailing comma ok
---
[a, b, c] = 1 ### "got int in sequence assignment"
---
[a, b] = [] ### "too few values to unpack"
---
[a, b] = [1] ### "too few values to unpack"
---
[a, b, c] = [1, 2] ### "too few values to unpack"
---
[a, b] = [1, 2, 3] ### "too many values to unpack"
---
[] = 1 ### "got int in sequence assignment"
---
[] = [1] ### "too many values to unpack"
---
[] = [1, 2] ### "too many values to unpack"
---
# list-tuple assignment
load("assert.star", "assert")

# empty ok
[] = ()
() = []

[a, b, c] = (1, 2, 3)
assert.eq(a, 1)
assert.eq(b, 2)
assert.eq(c, 3)

[a2, b2, c2] = 1, 2, 3 # bare tuple ok

(d, e, f) = [1, 2, 3]
assert.eq(d, 1)
assert.eq(e, 2)
assert.eq(f, 3)

[g, h, (i, j)] = (1, 2, [3, 4])
assert.eq(g, 1)
assert.eq(h, 2)
assert.eq(i, 3)
assert.eq(j, 4)

(k, l, [m, n]) = [1, 2, (3, 4)]
assert.eq(k, 1)
assert.eq(l, 2)
assert.eq(m, 3)
assert.eq(n, 4)

---
# option:nesteddef
# misc assignment
load("assert.star", "assert")

def assignment():
  a = [1, 2, 3]
  a[1] = 5
  assert.eq(a, [1, 5, 3])
  a[-2] = 2
  assert.eq(a, [1, 2, 3])
  assert.eq("%d %d" % (5, 7), "5 7")
  x={}
  x[1] = 2
  x[1] += 3
  assert.eq(x[1], 5)
  def f12(): x[(1, "abc", {})] = 1
  assert.fails(f12, "unhashable type: dict")

assignment()

---
# augmented assignment

load("assert.star", "assert")

def f():
  x = 1
  x += 1
  assert.eq(x, 2)
  x *= 3
  assert.eq(x, 6)
f()

---
# effects of evaluating LHS occur only once

load("assert.star", "assert")

count = [0] # count[0] is the number of calls to f

def f():
  count[0] += 1
  return count[0]

x = [1, 2, 3]
x[f()] += 1

assert.eq(x, [1, 3, 3]) # sole call to f returned 1
assert.eq(count[0], 1) # f was called only once

---
# Order of evaluation.

load("assert.star", "assert")

calls = []

def f(name, result):
  calls.append(name)
  return result

# The right side is evaluated before the left in an ordinary assignment.
calls.clear()
f("array", [0])[f("index", 0)] = f("rhs", 0)
assert.eq(calls, ["rhs", "array", "index"])

calls.clear()
f("lhs1", [0])[0], f("lhs2", [0])[0] = f("rhs1", 0), f("rhs2", 0)
assert.eq(calls, ["rhs1", "rhs2", "lhs1", "lhs2"])

# Left side is evaluated first (and only once) in an augmented assignment.
calls.clear()
f("array", [0])[f("index", 0)] += f("addend", 1)
assert.eq(calls, ["array", "index", "addend"])

---
# global referenced before assignment

def f():
   return g ### "global variable g referenced before assignment"

f()

g = 1

---
# option:nesteddef
# Free variables are captured by reference, so this is ok.
load("assert.star", "assert")

def f():
   def g():
     return outer
   outer = 1
   return g()

assert.eq(f(), 1)

---
load("assert.star", "assert")

printok = [False]

# This program should resolve successfully but fail dynamically.
# However, the Java implementation currently reports the dynamic
# error at the x=1 statement (b/33975425).  I think we need to simplify
# the resolver algorithm to what we have implemented.
def use_before_def():
  print(x) # dynamic error: local var referenced before assignment
  printok[0] = True
  x = 1  # makes 'x' local

assert.fails(use_before_def, 'local variable x referenced before assignment')
assert.true(not printok[0]) # execution of print statement failed

---
x = [1]
x.extend([2]) # ok

def f():
   x += [4] ### "local variable x referenced before assignment"

f()

---

z += 3 ### "global variable z referenced before assignment"

---
load("assert.star", "assert")

# It's ok to define a global that shadows a built-in...
list = []
assert.eq(type(list), "list")

# ...but then all uses refer to the global,
# even if they occur before the binding use.
# See github.com/google/skylark/issues/116.
assert.fails(lambda: tuple, "global variable tuple referenced before assignment")
tuple = ()

---
# option:set
# Same as above, but set is dialect-specific;
# we shouldn't notice any difference.
load("assert.star", "assert")

set = [1, 2, 3]
assert.eq(type(set), "list")

# As in Python 2 and Python 3,
# all 'in x' expressions in a comprehension are evaluated
# in the comprehension's lexical block, except the first,
# which is resolved in the outer block.
x = [[1, 2]]
assert.eq([x for x in x for y in x],
          [[1, 2], [1, 2]])

---
# A comprehension establishes a single new lexical block,
# not one per 'for' clause.
x = [1, 2]
_ = [x for _ in [3] for x in x] ### "local variable x referenced before assignment"

---
load("assert.star", "assert")

# assign singleton sequence to 1-tuple
(x,) = (1,)
assert.eq(x, 1)
(y,) = [1]
assert.eq(y, 1)

# assign 1-tuple to variable
z = (1,)
assert.eq(type(z), "tuple")
assert.eq(len(z), 1)
assert.eq(z[0], 1)

# assign value to parenthesized variable
(a) = 1
assert.eq(a, 1)

---
# assignment to/from fields.
load("assert.star", "assert", "freeze")

hf = hasfields()
hf.x = 1
assert.eq(hf.x, 1)
hf.x = [1, 2]
hf.x += [3, 4]
assert.eq(hf.x, [1, 2, 3, 4])
freeze(hf)
def setX(hf):
  hf.x = 2
def setY(hf):
  hf.y = 3
assert.fails(lambda: setX(hf), "cannot set field on a frozen hasfields")
assert.fails(lambda: setY(hf), "cannot set field on a frozen hasfields")

---
# destucturing assignment in a for loop.
load("assert.star", "assert")

def f():
  res = []
  for (x, y), z in [(["a", "b"], 3), (["c", "d"], 4)]:
    res.append((x, y, z))
  return res
assert.eq(f(), [("a", "b", 3), ("c", "d", 4)])

def g():
  a = {}
  for i, a[i] in [("one", 1), ("two", 2)]:
    pass
  return a
assert.eq(g(), {"one": 1, "two": 2})

---
# parenthesized LHS in augmented assignment (success)
# option:globalreassign
load("assert.star", "assert")

a = 5
(a) += 3
assert.eq(a, 8)

---
# parenthesized LHS in augmented assignment (error)

(a) += 5 ### "global variable a referenced before assignment"

---
# option:globalreassign
load("assert.star", "assert")
assert = 1
load("assert.star", "assert")

---
# option:globalreassign option:loadbindsglobally
load("assert.star", "assert")
assert = 1
load("assert.star", "assert")

---
# option:loadbindsglobally
_ = assert ### "global variable assert referenced before assignment"
load("assert.star", "assert")

---
_ = assert ### "local variable assert referenced before assignment"
load("assert.star", "assert")

---
def f(): assert.eq(1, 1) # forward ref OK
load("assert.star", "assert")
f()

---
# option:loadbindsglobally
def f(): assert.eq(1, 1) # forward ref OK
load("assert.star", "assert")
f()
