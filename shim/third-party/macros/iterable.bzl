# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

# @nolint

"""Provides utility methods for working with iterable collections."""

load(":type_defs.bzl", "is_list")

def _product(*args):
    """Returns a Cartesian product of provided `*args`.

    The result is the same as the list of lists produced by nested loops going
    through every passed collection:

        result = []
        for x in arg1:
            for y in arg2:
                ...
                    return result.append([x, y, ...])

    For example, for [['a', 'b'], [1, 2]] the function will return
    [['a', 1], ['a', 2], ['b', 1] ['b', 2]].

    Note that unlike Python's `itertools.product` the result is a list and as
    such an entire result is materialized in memory, so clients should use
    this method with care to avoid excessive memory usage.

    Another difference with Python's implementation is that instead of tuples
    Cartesian coordinates are returned as lists instead of tuples for
    efficiency reasons.

    Args:
      *args: iterable instances (dimensions) of a certesian product

    Returns:
      Cartesian product of provided arguments.
    """
    product = [[]]

    for arg in args:
        product = [p + [e] for p in product for e in arg]

    return product

def _reduce(function, iterable, initializer = None):
    """Accumulates all elements of the iterable using a function.

    The process for a sequence (s1, s2, s3, ..., sn) and function fn looks
    something like (...((s1 fn s2) fn s3) + ... + sn) and is also known the
    right fold.

    If initializer is provided, it replaces the first element of the sequence.

    Args:
      function: accumulator function
      iterable: a sequence of elements to accumulate
      initializer: an optional value to use as the first element in the
        sequence. This value will also be returned in case of an empty sequence.

    Returns:
      an accumulated value
    """
    if not iterable and not initializer:
        fail("Cannot reduce an empty iterable without initializer.")
    value, initialized = initializer, initializer != None
    for item in iterable:
        if not initialized:
            value, initialized = item, True
        else:
            value = function(value, item)
    return value

def _first_non_none(iterable, default = None):
    """Return the first non-`None` value in an iterable.

    Args:
      iterable: a sequence of elements
      default: value to use if iterable contains no non-`None` values

    Returns:
      the first non-value in the given iterable or the default value
    """
    for item in iterable:
        if item != None:
            return item
    return default

def _to_list(iterable):
    """
    Convert the iterable to a list, or return the original object if already a list

    This is generally useful for things like `.keys()` which might return a dict_keys
    view in python3, but a list in python2 (and starlark)
    """
    if is_list(iterable):
        return iterable
    return list(iterable)

def _first(iterable):
    """ Get the first element in an iterable (that may not be indexable) """
    for val in iterable:
        return val
    fail("No values found in %r" % (iterable,))

def _uniq_list(*iterables):
    """ Combine all of the unique elements of the iterables into a single list """
    result = {
        val: None
        for iterable in iterables
        for val in iterable
    }
    return _to_list(result)

def _concat(*iterables):
    """ Join several iterables (that may not be indexable) into a list """
    result = []
    for iterable in iterables:
        result.extend(iterable)
    return result

iterable = struct(
    first_non_none = _first_non_none,
    product = _product,
    reduce = _reduce,
    to_list = _to_list,
    first = _first,
    uniq_list = _uniq_list,
    concat = _concat,
)
