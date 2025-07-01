# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def by_platform(
        platform_flavors: list[str],
        xs: list[(str, typing.Any)]) -> list[typing.Any]:
    """
    Resolve platform-flavor-specific parameters, given the list of platform
    flavors to match against.  Meant to mirror the usage of
    `PatternMatchedCollection`s in v1 for `platform_*` parameters.
    """

    res = []

    for (dtype, deps) in xs:
        for platform in platform_flavors:
            if regex_match(dtype, platform):
                res.append(deps)

    return res
