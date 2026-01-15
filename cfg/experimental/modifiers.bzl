# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cfg/modifier:asserts.bzl", "verify_normalized_modifier", "verify_normalized_target")
load(
    "@prelude//cfg/modifier:types.bzl",
    "Modifier",  # @unused Used in type annotation
    "ModifiersMatch",
)

_DIGIT_TO_ALPHA = {
    "0": "a",
    "1": "b",
    "2": "c",
    "3": "d",
    "4": "e",
    "5": "f",
    "6": "g",
    "7": "h",
    "8": "i",
    "9": "j",
}

def _int_to_alpha(n):
    """Convert an integer to a lowercase alphabetical string by mapping each digit to a letter."""
    if n < 0:
        n = -n
    digits = str(n)
    result = [_DIGIT_TO_ALPHA[digits[i]] for i in range(len(digits))]
    return "".join(result)

def _modifiers_match(
        matcher: dict[str, Modifier]) -> ModifiersMatch:
    """
    A select operator for modifiers. A `modifiers.match` specifies a way for a
    modifier to be added based on an existing constraint in the configuration.
    The `matcher` is a dictionary that maps from a set of constraints to a
    modifier.

    For example, suppose `cfg//os:linux` and `cfg//os:windows` are constraint values
    for OS and `cfg//compiler:clang` and `cfg//compiler:msvc` are constraint values
    for compiler. The following `modifiers.match` conditionally adds the msvc constraint
    if the the windows constraint is matched or adds the clang constraint if the
    the linux constraint is matched.
    ```
    modifiers.match({
        "cfg//os:windows": "cfg//compiler:msvc",
        "cfg//os:linux": "cfg//compiler:clang",
        "DEFAULT": None,
    })
    ```
    "DEFAULT" is a special key that represents the default case. If no other keys match,
    then the modifier specified by DEFAULT will be used.
    If None is specified, then a modifier will not be added.

    `modifiers.match`s can be stacked. For example,
    suppose this modifier is specified in fbcode/PACKAGE
    ```
    modifier = modifiers.match({
        "cfg//os:windows": "cfg//compiler:msvc",
        "DEFAULT": None,
    })
    ```
    Suppose this modifier is specified in fbcode/project/PACKAGE
    ```
    modifier = modifiers.match({
        "cfg//os:linux": "cfg//compiler:clang",
        "DEFAULT": None,
    })
    ```
    For any target covered by fbcode/project/PACKAGE, this is
    equivalent to one modifier in that specifies
    ```
    modifiers.match({
        "cfg//os:windows": "cfg//compiler:msvc",
        "DEFAULT": modifiers.match({
            "DEFAULT": None,
            "cfg//os:linux": "cfg//compiler:clang",
        })
    })
    ```
    """

    for key, sub_modifier in matcher.items():
        if key != "DEFAULT":
            verify_normalized_target(key)
        verify_normalized_modifier(sub_modifier)

    matcher_str = str(matcher)
    soft_error("starlark_conditional_modifier_{}".format(
        # We use a hash so we log a different soft error category for each unique conditional modifier.
        # We can only use alphabetical characters in soft error category due to logview restrictions,
        # so convert integers from hash to alphabetical characters.
        _int_to_alpha(hash(matcher_str)),
    ), matcher_str, quiet = True)

    matcher["_type"] = "ModifiersMatch"

    return matcher

modifiers = struct(
    # modifiers.match is deprecated for modifiers.conditional
    match = _modifiers_match,
    conditional = _modifiers_match,
)
