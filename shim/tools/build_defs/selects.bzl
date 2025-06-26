load("@prelude//utils:selects.bzl", _prelude_selects = "selects")

def _cond(condition: str) -> Select:
    """Returns a select that evaluates to True if the condition is met.

    Useful for terse logical conditions.

    For example,
    ```
    selects.if_(selects.and(selects.cond("//config:linux"), selects.cond("//config:clang")), ["-g1"], ["-g2"])
    ```
    """
    return select({
        "DEFAULT": False,
        condition: True,
    })

def _and(val, *vals):
    if not vals:
        return val
    return _prelude_selects.apply(
        val,
        lambda val: val and _and(*vals),
    )

def _or(val, *vals):
    if not vals:
        return val
    return _prelude_selects.apply(
        val,
        lambda val: val or _or(*vals),
    )

def _if(cond, a, b):
    return _prelude_selects.apply(
        cond,
        native.partial(_if_inner, a = a, b = b),
    )

def _if_inner(cond, a, b):
    return a if cond else b

selects = struct(
    and_ = _and,
    cond = _cond,
    or_ = _or,
    if_ = _if,
)
