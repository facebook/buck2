# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

_SELECT_TYPE = type(select({"DEFAULT": []}))

def _is_select(thing):
    return type(thing) == _SELECT_TYPE

def _apply(obj, function):
    """
    If the object is a select, runs `select_map` with `function`.
    Otherwise, if the object is not a select, invokes `function` on `obj` directly.
    """
    if not _is_select(obj):
        return function(obj)
    return select_map(
        obj,
        lambda obj: _apply(obj, function),
    )

def _tie_n_impl_inner(objs, pvals, val):
    return _tie_n_impl(objs[1:], pvals + [val])

def _tie_n_impl(objs, pvals):
    if not objs:
        return tuple(pvals)

    return _apply(
        objs[0],
        partial(_tie_n_impl_inner, objs, pvals),
    )

def _tie_n(*objs):
    """
    Combine separate select objects into a single select with all values of
    the select "tied" into n-tuples.

    Arguments:
        objs: Objects which may be behind (possible nested) `select`s.

    Returns: A `select` resolving to an n-tuple of the passed in object
    """
    return _tie_n_impl(objs, [])

def _apply_n_inner(func, vals):
    return func(*vals)

def _apply_n(objs, func):
    """
    Return a new `select` formed by applying the given function to all possible
    combinations of the given select objects.
    """
    return _apply(
        _tie_n(*objs),
        # Unpack n-tuple and call user-supplied function.
        partial(_apply_n_inner, func),
    )

selects = struct(
    apply = _apply,
    apply_n = _apply_n,
)
