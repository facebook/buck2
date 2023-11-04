# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    ":types.bzl",
    "CfgModifier",  # @unused Used in type annotation
    "ModifierSelect",
)

def modifier_select(
        selector: dict[str, CfgModifier | None]) -> ModifierSelect:
    """
    A select operator for modifiers. A `modifier_select` specifies a way for a
    modifier to be added based on an existing constraint in the configuration.
    The `selector` is a dictionary that maps from a set of constraints to a
    modifier.

    For example, suppose `cfg//os:linux` and `cfg//os:windows` are constraint values
    for OS and `cfg//compiler:clang` and `cfg//compiler:msvc` are constraint values
    for compiler. The following `modifier_select` conditionally adds the msvc constraint
    if the the windows constraint is matched or adds the clang constraint if the
    the linux constraint is matched.
    ```
    modifier_select({
        "cfg//os:windows": "cfg//compiler:msvc",
        "cfg//os:linux": "cfg//compiler:clang",
        "DEFAULT": None,
    })
    ```
    "DEFAULT" is a special key that represents the default case. If no other keys match,
    then the modifier specified by DEFAULT will be used.
    If None is specified, then a modifier will not be added.

    `modifier_select`s can be stacked. For example,
    suppose this modifier is specified in fbcode/PACKAGE
    ```
    modifier = modifier_select({
        "cfg//os:windows": "cfg//compiler:msvc",
        "DEFAULT": None,
    })
    ```
    Suppose this modifier is specified in fbcode/project/PACKAGE
    ```
    modifier = modifier_select({
        "cfg//os:linux": "cfg//compiler:clang",
        "DEFAULT": None,
    })
    ```
    For any target covered by fbcode/project/PACKAGE, this is
    equivalent to one modifier in that specifies
    ```
    modifier_select({
        "cfg//os:windows": "cfg//compiler:msvc",
        "DEFAULT": modifier_select({
            "DEFAULT": None,
            "cfg//os:linux": "cfg//compiler:clang",
        })
    })
    ```
    """

    return ModifierSelect(selector = selector)
