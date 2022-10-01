# @generated
# Copied from https://github.com/google/starlark-go/blob/87f333178d5942de51b193111d6f636c79833ea5/starlark/testdata/

# Tests of Starlark 'float'
# option:set

load("assert.star", "assert")

# TODO(adonovan): more tests:
# - precision
# - limits

# type
assert.eq(type(0.0), "float")

# truth
assert.true(123.0)
assert.true(-1.0)
assert.true(not 0.0)
assert.true(-1.0e-45)
assert.true(float("NaN"))

# not iterable
assert.fails(lambda: len(0.0), 'has no len')
assert.fails(lambda: [x for x in 0.0], 'float value is not iterable')

# literals
assert.eq(type(1.234), "float")
assert.eq(type(1e10), "float")
assert.eq(type(1e+10), "float")
assert.eq(type(1e-10), "float")
assert.eq(type(1.234e10), "float")
assert.eq(type(1.234e+10), "float")
assert.eq(type(1.234e-10), "float")

# int/float equality
assert.eq(0.0, 0)
assert.eq(0, 0.0)
assert.eq(1.0, 1)
assert.eq(1, 1.0)
assert.true(1.23e45 != 1229999999999999973814869011019624571608236031)
assert.true(1.23e45 == 1229999999999999973814869011019624571608236032)
assert.true(1.23e45 != 1229999999999999973814869011019624571608236033)
assert.true(1229999999999999973814869011019624571608236031 != 1.23e45)
assert.true(1229999999999999973814869011019624571608236032 == 1.23e45)
assert.true(1229999999999999973814869011019624571608236033 != 1.23e45)

# loss of precision
p53 = 1<<53
assert.eq(float(p53-1), p53-1)
assert.eq(float(p53+0), p53+0)
assert.eq(float(p53+1), p53+0) #
assert.eq(float(p53+2), p53+2)
assert.eq(float(p53+3), p53+4) #
assert.eq(float(p53+4), p53+4)
assert.eq(float(p53+5), p53+4) #
assert.eq(float(p53+6), p53+6)
assert.eq(float(p53+7), p53+8) #
assert.eq(float(p53+8), p53+8)

# Regression test for https://github.com/google/starlark-go/issues/375.
maxint64 = (1<<63)-1
assert.eq(int(float(maxint64)), 9223372036854775808)

assert.true(float(p53+1) != p53+1) # comparisons are exact
assert.eq(float(p53+1) - (p53+1), 0) # arithmetic entails rounding

assert.fails(lambda: {123.0: "f", 123: "i"}, "duplicate key: 123")

# equal int/float values have same hash
d = {123.0: "x"}
d[123] = "y"
assert.eq(len(d), 1)
assert.eq(d[123.0], "y")

# literals (mostly covered by scanner tests)
assert.eq(str(0.), "0.0")
assert.eq(str(.0), "0.0")
assert.true(5.0 != 4.999999999999999)
assert.eq(5.0, 4.9999999999999999) # both literals denote 5.0
assert.eq(1.23e45, 1.23 * 1000000000000000000000000000000000000000000000)
assert.eq(str(1.23e-45 - (1.23 / 1000000000000000000000000000000000000000000000)), "-1.5557538194652854e-61")

nan = float("NaN")
inf = float("+Inf")
neginf = float("-Inf")
negzero = (-1e-323 / 10)

# -- arithmetic --

# +float, -float
assert.eq(+(123.0), 123.0)
assert.eq(-(123.0), -123.0)
assert.eq(-(-(123.0)), 123.0)
assert.eq(+(inf), inf)
assert.eq(-(inf), neginf)
assert.eq(-(neginf), inf)
assert.eq(str(-(nan)), "nan")
# +
assert.eq(1.2e3 + 5.6e7, 5.60012e+07)
assert.eq(1.2e3 + 1, 1201)
assert.eq(1 + 1.2e3, 1201)
assert.eq(str(1.2e3 + nan), "nan")
assert.eq(inf + 0, inf)
assert.eq(inf + 1, inf)
assert.eq(inf + inf, inf)
assert.eq(str(inf + neginf), "nan")
# -
assert.eq(1.2e3 - 5.6e7, -5.59988e+07)
assert.eq(1.2e3 - 1, 1199)
assert.eq(1 - 1.2e3, -1199)
assert.eq(str(1.2e3 - nan), "nan")
assert.eq(inf - 0, inf)
assert.eq(inf - 1, inf)
assert.eq(str(inf - inf), "nan")
assert.eq(inf - neginf, inf)
# *
assert.eq(1.5e6 * 2.2e3, 3.3e9)
assert.eq(1.5e6 * 123, 1.845e+08)
assert.eq(123 * 1.5e6, 1.845e+08)
assert.eq(str(1.2e3 * nan), "nan")
assert.eq(str(inf * 0), "nan")
assert.eq(inf * 1, inf)
assert.eq(inf * inf, inf)
assert.eq(inf * neginf, neginf)
# %
assert.eq(100.0 % 7.0, 2)
assert.eq(100.0 % -7.0, -5) # NB: different from Go / Java
assert.eq(-100.0 % 7.0, 5) # NB: different from Go / Java
assert.eq(-100.0 % -7.0, -2)
assert.eq(-100.0 % 7, 5)
assert.eq(100 % 7.0, 2)
assert.eq(str(1.2e3 % nan), "nan")
assert.eq(str(inf % 1), "nan")
assert.eq(str(inf % inf), "nan")
assert.eq(str(inf % neginf), "nan")
# /
assert.eq(str(100.0 / 7.0), "14.285714285714286")
assert.eq(str(100 / 7.0), "14.285714285714286")
assert.eq(str(100.0 / 7), "14.285714285714286")
assert.eq(str(100.0 / nan), "nan")
# //
assert.eq(100.0 // 7.0, 14)
assert.eq(100 // 7.0, 14)
assert.eq(100.0 // 7, 14)
assert.eq(100.0 // -7.0, -15)
assert.eq(100 // -7.0, -15)
assert.eq(100.0 // -7, -15)
assert.eq(str(1 // neginf), "-0.0")
assert.eq(str(100.0 // nan), "nan")

# addition
assert.eq(0.0 + 1.0, 1.0)
assert.eq(1.0 + 1.0, 2.0)
assert.eq(1.25 + 2.75, 4.0)
assert.eq(5.0 + 7.0, 12.0)
assert.eq(5.1 + 7, 12.1)  # float + int
assert.eq(7 + 5.1, 12.1)  # int + float

# subtraction
assert.eq(5.0 - 7.0, -2.0)
assert.eq(5.1 - 7.1, -2.0)
assert.eq(5.5 - 7, -1.5)
assert.eq(5 - 7.5, -2.5)
assert.eq(0.0 - 1.0, -1.0)

# multiplication
assert.eq(5.0 * 7.0, 35.0)
assert.eq(5.5 * 2.5, 13.75)
assert.eq(5.5 * 7, 38.5)
assert.eq(5 * 7.1, 35.5)

# real division (like Python 3)
# The / operator is available only when the 'fp' dialect option is enabled.
assert.eq(100.0 / 8.0, 12.5)
assert.eq(100.0 / -8.0, -12.5)
assert.eq(-100.0 / 8.0, -12.5)
assert.eq(-100.0 / -8.0, 12.5)
assert.eq(98.0 / 8.0, 12.25)
assert.eq(98.0 / -8.0, -12.25)
assert.eq(-98.0 / 8.0, -12.25)
assert.eq(-98.0 / -8.0, 12.25)
assert.eq(2.5 / 2.0, 1.25)
assert.eq(2.5 / 2, 1.25)
assert.eq(5 / 4.0, 1.25)
assert.eq(5 / 4, 1.25)
assert.fails(lambda: 1.0 / 0, "floating-point division by zero")
assert.fails(lambda: 1.0 / 0.0, "floating-point division by zero")
assert.fails(lambda: 1 / 0.0, "floating-point division by zero")

# floored division
assert.eq(100.0 // 8.0, 12.0)
assert.eq(100.0 // -8.0, -13.0)
assert.eq(-100.0 // 8.0, -13.0)
assert.eq(-100.0 // -8.0, 12.0)
assert.eq(98.0 // 8.0, 12.0)
assert.eq(98.0 // -8.0, -13.0)
assert.eq(-98.0 // 8.0, -13.0)
assert.eq(-98.0 // -8.0, 12.0)
assert.eq(2.5 // 2.0, 1.0)
assert.eq(2.5 // 2, 1.0)
assert.eq(5 // 4.0, 1.0)
assert.eq(5 // 4, 1)
assert.eq(type(5 // 4), "int")
assert.fails(lambda: 1.0 // 0, "floored division by zero")
assert.fails(lambda: 1.0 // 0.0, "floored division by zero")
assert.fails(lambda: 1 // 0.0, "floored division by zero")

# remainder
assert.eq(100.0 % 8.0, 4.0)
assert.eq(100.0 % -8.0, -4.0)
assert.eq(-100.0 % 8.0, 4.0)
assert.eq(-100.0 % -8.0, -4.0)
assert.eq(98.0 % 8.0, 2.0)
assert.eq(98.0 % -8.0, -6.0)
assert.eq(-98.0 % 8.0, 6.0)
assert.eq(-98.0 % -8.0, -2.0)
assert.eq(2.5 % 2.0, 0.5)
assert.eq(2.5 % 2, 0.5)
assert.eq(5 % 4.0, 1.0)
assert.fails(lambda: 1.0 % 0, "floating-point modulo by zero")
assert.fails(lambda: 1.0 % 0.0, "floating-point modulo by zero")
assert.fails(lambda: 1 % 0.0, "floating-point modulo by zero")

# floats cannot be used as indices, even if integral
assert.fails(lambda: "abc"[1.0], "want int")
assert.fails(lambda: ["A", "B", "C"].insert(1.0, "D"), "want int")
assert.fails(lambda: range(3)[1.0], "got float, want int")

# -- comparisons --
# NaN
assert.true(nan == nan) # \
assert.true(nan >= nan) #  unlike Python
assert.true(nan <= nan) # /
assert.true(not (nan > nan))
assert.true(not (nan < nan))
assert.true(not (nan != nan)) # unlike Python
# Sort is stable: 0.0 and -0.0 are equal, but they are not permuted.
# Similarly 1 and 1.0.
assert.eq(
    str(sorted([inf, neginf, nan, 1e300, -1e300, 1.0, -1.0, 1, -1, 1e-300, -1e-300, 0, 0.0, negzero, 1e-300, -1e-300])),
    "[-inf, -1e+300, -1.0, -1, -1e-300, -1e-300, 0, 0.0, -0.0, 1e-300, 1e-300, 1.0, 1, 1e+300, +inf, nan]")

# Sort is stable, and its result contains no adjacent x, y such that y > x.
# Note: Python's reverse sort is unstable; see https://bugs.python.org/issue36095.
assert.eq(str(sorted([7, 3, nan, 1, 9])), "[1, 3, 7, 9, nan]")
assert.eq(str(sorted([7, 3, nan, 1, 9], reverse=True)), "[nan, 9, 7, 3, 1]")

# All NaN values compare equal. (Identical objects compare equal.)
nandict = {nan: 1}
nandict[nan] = 2
assert.eq(len(nandict), 1) # (same as Python)
assert.eq(nandict[nan], 2) # (same as Python)
assert.fails(lambda: {nan: 1, nan: 2}, "duplicate key: nan")

nandict[float('nan')] = 3 # a distinct NaN object
assert.eq(str(nandict), "{nan: 3}") # (Python: {nan: 2, nan: 3})

assert.eq(str({inf: 1, neginf: 2}), "{+inf: 1, -inf: 2}")

# zero
assert.eq(0.0, negzero)

# inf
assert.eq(+inf / +inf, nan)
assert.eq(+inf / -inf, nan)
assert.eq(-inf / +inf, nan)
assert.eq(0.0 / +inf, 0.0)
assert.eq(0.0 / -inf, 0.0)
assert.true(inf > -inf)
assert.eq(inf, -neginf)
# TODO(adonovan): assert inf > any finite number, etc.

# negative zero
negz = -0
assert.eq(negz, 0)

# min/max ordering with NaN (the greatest float value)
assert.eq(max([1, nan, 3]), nan)
assert.eq(max([nan, 2, 3]), nan)
assert.eq(min([1, nan, 3]), 1)
assert.eq(min([nan, 2, 3]), 2)

# float/float comparisons
fltmax = 1.7976931348623157e+308 # approx
fltmin = 4.9406564584124654e-324 # approx
assert.lt(-inf, -fltmax)
assert.lt(-fltmax, -1.0)
assert.lt(-1.0, -fltmin)
assert.lt(-fltmin, 0.0)
assert.lt(0, fltmin)
assert.lt(fltmin, 1.0)
assert.lt(1.0, fltmax)
assert.lt(fltmax, inf)

# int/float comparisons
assert.eq(0, 0.0)
assert.eq(1, 1.0)
assert.eq(-1, -1.0)
assert.ne(-1, -1.0 + 1e-7)
assert.lt(-2, -2 + 1e-15)

# int conversion (rounds towards zero)
assert.eq(int(100.1), 100)
assert.eq(int(100.0), 100)
assert.eq(int(99.9), 99)
assert.eq(int(-99.9), -99)
assert.eq(int(-100.0), -100)
assert.eq(int(-100.1), -100)
assert.eq(int(1e100), int("10000000000000000159028911097599180468360808563945281389781327557747838772170381060813469985856815104"))
assert.fails(lambda: int(inf), "cannot convert.*infinity")
assert.fails(lambda: int(nan), "cannot convert.*NaN")

# -- float() function --
assert.eq(float(), 0.0)
# float(bool)
assert.eq(float(False), 0.0)
assert.eq(float(True), 1.0)
# float(int)
assert.eq(float(0), 0.0)
assert.eq(float(1), 1.0)
assert.eq(float(123), 123.0)
assert.eq(float(123 * 1000000 * 1000000 * 1000000 * 1000000 * 1000000), 1.23e+32)
# float(float)
assert.eq(float(1.1), 1.1)
assert.fails(lambda: float(None), "want number or string")
assert.ne(False, 0.0) # differs from Python
assert.ne(True, 1.0)
# float(string)
assert.eq(float("1.1"), 1.1)
assert.fails(lambda: float("1.1abc"), "invalid float literal")
assert.fails(lambda: float("1e100.0"), "invalid float literal")
assert.fails(lambda: float("1e1000"), "floating-point number too large")
assert.eq(float("-1.1"), -1.1)
assert.eq(float("+1.1"), +1.1)
assert.eq(float("+Inf"), inf)
assert.eq(float("-Inf"), neginf)
assert.eq(float("NaN"), nan)
assert.eq(float("NaN"), nan)
assert.eq(float("+NAN"), nan)
assert.eq(float("-nan"), nan)
assert.eq(str(float("Inf")), "+inf")
assert.eq(str(float("+INF")), "+inf")
assert.eq(str(float("-inf")), "-inf")
assert.eq(str(float("+InFiniTy")), "+inf")
assert.eq(str(float("-iNFiniTy")), "-inf")
assert.fails(lambda: float("one point two"), "invalid float literal: one point two")
assert.fails(lambda: float("1.2.3"), "invalid float literal: 1.2.3")
assert.fails(lambda: float(123 << 500 << 500 << 50), "int too large to convert to float")
assert.fails(lambda: float(-123 << 500 << 500 << 50), "int too large to convert to float")
assert.fails(lambda: float(str(-123 << 500 << 500 << 50)), "floating-point number too large")

# -- implicit float(int) conversions --
assert.fails(lambda: (1<<500<<500<<500) + 0.0, "int too large to convert to float")
assert.fails(lambda: 0.0 + (1<<500<<500<<500), "int too large to convert to float")
assert.fails(lambda: (1<<500<<500<<500) - 0.0, "int too large to convert to float")
assert.fails(lambda: 0.0 - (1<<500<<500<<500), "int too large to convert to float")
assert.fails(lambda: (1<<500<<500<<500) * 1.0, "int too large to convert to float")
assert.fails(lambda: 1.0 * (1<<500<<500<<500), "int too large to convert to float")
assert.fails(lambda: (1<<500<<500<<500) / 1.0, "int too large to convert to float")
assert.fails(lambda: 1.0 / (1<<500<<500<<500), "int too large to convert to float")
assert.fails(lambda: (1<<500<<500<<500) // 1.0, "int too large to convert to float")
assert.fails(lambda: 1.0 // (1<<500<<500<<500), "int too large to convert to float")
assert.fails(lambda: (1<<500<<500<<500) % 1.0, "int too large to convert to float")
assert.fails(lambda: 1.0 % (1<<500<<500<<500), "int too large to convert to float")


# -- int function --
assert.eq(int(0.0), 0)
assert.eq(int(1.0), 1)
assert.eq(int(1.1), 1)
assert.eq(int(0.9), 0)
assert.eq(int(-1.1), -1.0)
assert.eq(int(-1.0), -1.0)
assert.eq(int(-0.9), 0.0)
assert.eq(int(1.23e+32), 123000000000000004979083645550592)
assert.eq(int(-1.23e-32), 0)
assert.eq(int(1.23e-32), 0)
assert.fails(lambda: int(float("+Inf")), "cannot convert float infinity to integer")
assert.fails(lambda: int(float("-Inf")), "cannot convert float infinity to integer")
assert.fails(lambda: int(float("NaN")), "cannot convert float NaN to integer")


# hash
# Check that equal float and int values have the same internal hash.
def checkhash():
  for a in [
            1.23e100,   # int overflow in starlark-rust
            1.23e10,    # int overflow in starlark-rust
            1.23e1, 1.23, 1,
            4294967295, 8589934591, 9223372036854775807, # int overflow in starlark-rust
            ]:
    for b in [a, -a, 1/a, -1/a]:
      f = float(b)
      i = int(b)
      if f == i:
        fh = {f: None}
        ih = {i: None}
        if fh != ih:
          assert.true(False, "{%v: None} != {%v: None}: hashes vary" % fh, ih)
checkhash()

# string formatting

# %d
assert.eq("%d" % 0, "0")
assert.eq("%d" % 0.0, "0")
assert.eq("%d" % 123, "123")
assert.eq("%d" % 123.0, "123")
assert.eq("%d" % 1.23e45, "1229999999999999973814869011019624571608236032")
# (see below for '%d' % NaN/Inf)
assert.eq("%d" % negzero, "0")
assert.fails(lambda: "%d" % float("NaN"), "cannot convert float NaN to integer")
assert.fails(lambda: "%d" % float("+Inf"), "cannot convert float infinity to integer")
assert.fails(lambda: "%d" % float("-Inf"), "cannot convert float infinity to integer")

# %e
assert.eq("%e" % 0, "0.000000e+00")
assert.eq("%e" % 0.0, "0.000000e+00")
assert.eq("%e" % 123, "1.230000e+02")
assert.eq("%e" % 123.0, "1.230000e+02")
assert.eq("%e" % 1.23e45, "1.230000e+45")
assert.eq("%e" % -1.23e-45, "-1.230000e-45")
assert.eq("%e" % nan, "nan")
assert.eq("%e" % inf, "+inf")
assert.eq("%e" % neginf, "-inf")
assert.eq("%e" % negzero, "-0.000000e+00")
assert.fails(lambda: "%e" % "123", "requires float, not str")
# %f
assert.eq("%f" % 0, "0.000000")
assert.eq("%f" % 0.0, "0.000000")
assert.eq("%f" % 123, "123.000000")
assert.eq("%f" % 123.0, "123.000000")
# Note: Starlark/Java emits 1230000000000000000000000000000000000000000000.000000. Why?
assert.eq("%f" % 1.23e45, "1229999999999999973814869011019624571608236032.000000")
assert.eq("%f" % -1.23e-45, "-0.000000")
assert.eq("%f" % nan, "nan")
assert.eq("%f" % inf, "+inf")
assert.eq("%f" % neginf, "-inf")
assert.eq("%f" % negzero, "-0.000000")
assert.fails(lambda: "%f" % "123", "requires float, not str")
# %g
assert.eq("%g" % 0, "0.0")
assert.eq("%g" % 0.0, "0.0")
assert.eq("%g" % 123, "123.0")
assert.eq("%g" % 123.0, "123.0")
assert.eq("%g" % 1.110, "1.11")
assert.eq("%g" % 1e5, "100000.0")
assert.eq("%g" % 1e6, "1e+06") # Note: threshold of scientific notation is 1e17 in Starlark/Java
assert.eq("%g" % 1.23e45, "1.23e+45")
assert.eq("%g" % -1.23e-45, "-1.23e-45")
assert.eq("%g" % nan, "nan")
assert.eq("%g" % inf, "+inf")
assert.eq("%g" % neginf, "-inf")
assert.eq("%g" % negzero, "-0.0")
# str uses %g
assert.eq(str(0.0), "0.0")
assert.eq(str(123.0), "123.0")
assert.eq(str(1.23e45), "1.23e+45")
assert.eq(str(-1.23e-45), "-1.23e-45")
assert.eq(str(nan), "nan")
assert.eq(str(inf), "+inf")
assert.eq(str(neginf), "-inf")
assert.eq(str(negzero), "-0.0")
assert.fails(lambda: "%g" % "123", "requires float, not str")

i0 = 1
f0 = 1.0
assert.eq(type(i0), "int")
assert.eq(type(f0), "float")

ops = {
    '+': lambda x, y: x + y,
    '-': lambda x, y: x - y,
    '*': lambda x, y: x * y,
    '/': lambda x, y: x / y,
    '//': lambda x, y: x // y,
    '%': lambda x, y: x % y,
}

# Check that if either argument is a float, so too is the result.
def checktypes():
  want = set("""
int + int = int
int + float = float
float + int = float
float + float = float
int - int = int
int - float = float
float - int = float
float - float = float
int * int = int
int * float = float
float * int = float
float * float = float
int / int = float
int / float = float
float / int = float
float / float = float
int // int = int
int // float = float
float // int = float
float // float = float
int % int = int
int % float = float
float % int = float
float % float = float
"""[1:].splitlines())
  for opname in ("+", "-", "*", "/", "%"):
    for x in [i0, f0]:
      for y in [i0, f0]:
        op = ops[opname]
        got = "%s %s %s = %s" % (type(x), opname, type(y), type(op(x, y)))
        assert.contains(want, got)
checktypes()
