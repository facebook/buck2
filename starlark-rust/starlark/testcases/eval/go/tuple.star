# @generated
# Copied from https://github.com/google/starlark-go/blob/e81fc95f7bd5bb1495fe69f27c1a99fcc77caa48/starlark/testdata/

# Tests of Starlark 'tuple'

load("assert.star", "assert")

# literal
assert.eq((), ())
assert.eq((1), 1)
assert.eq((1,), (1,))
assert.ne((1), (1,))
assert.eq((1, 2), (1, 2))
assert.eq((1, 2, 3, 4, 5), (1, 2, 3, 4, 5))
assert.ne((1, 2, 3), (1, 2, 4))

# truth
assert.true((False,))
assert.true((False, False))
assert.true(not ())

# indexing, x[i]
assert.eq(("a", "b")[0], "a")
assert.eq(("a", "b")[1], "b")

# slicing, x[i:j]
assert.eq("abcd"[0:4:1], "abcd")
assert.eq("abcd"[::2], "ac")
assert.eq("abcd"[1::2], "bd")
assert.eq("abcd"[4:0:-1], "dcb")
banana = tuple("banana".elems())
assert.eq(banana[7::-2], tuple("aaa".elems()))
assert.eq(banana[6::-2], tuple("aaa".elems()))
assert.eq(banana[5::-2], tuple("aaa".elems()))
assert.eq(banana[4::-2], tuple("nnb".elems()))

# tuple
assert.eq(tuple(), ())
assert.eq(tuple("abc".elems()), ("a", "b", "c"))
assert.eq(tuple(["a", "b", "c"]), ("a", "b", "c"))
assert.eq(tuple([1]), (1,))
assert.fails(lambda: tuple(1), "got int, want iterable")

# tuple * int,  int * tuple
abc = tuple("abc".elems())
assert.eq(abc * 0, ())
assert.eq(abc * -1, ())
assert.eq(abc * 1, abc)
assert.eq(abc * 3, ("a", "b", "c", "a", "b", "c", "a", "b", "c"))
assert.eq(0 * abc, ())
assert.eq(-1 * abc, ())
assert.eq(1 * abc, abc)
assert.eq(3 * abc, ("a", "b", "c", "a", "b", "c", "a", "b", "c"))
assert.fails(lambda: abc * (1000000 * 1000000), "repeat count 1000000000000 too large")
assert.fails(lambda: abc * 1000000 * 1000000, "excessive repeat \\(3000000 \\* 1000000 elements")

# TODO(adonovan): test use of tuple as sequence
# (for loop, comprehension, library functions).
