# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_configured_attribute_filters(buck: Buck) -> None:
    cases = [
        ("attrfilter(text, hello, root//:plain)", {"root//:plain"}),
        ("attrregexfilter(text, '^he.lo$', root//:plain)", {"root//:plain"}),
        ("attrfilter(strings, last, root//:plain)", {"root//:plain"}),
        ("attrfilter(strings, missing, root//:plain)", set()),
        ("attrfilter(text, fallback, root//:defaults)", {"root//:defaults"}),
        ("attrfilter(strings, anything, root//:defaults)", set()),
        ("attrfilter(missing_attribute, anything, root//:plain)", set()),
        ("attrfilter(strings, prefix, root//:selected)", {"root//:selected"}),
        ("attrfilter(strings, selected, root//:selected)", {"root//:selected"}),
        ("attrfilter(strings, unselected, root//:selected)", set()),
        ("attrfilter(text, helloworld, root//:selected)", {"root//:selected"}),
        ("attrfilter(text, hello, root//:selected)", set()),
        ("attrfilter(mixed, nested, root//:mixed)", {"root//:mixed"}),
        ("attrfilter(mixed, '17', root//:mixed)", {"root//:mixed"}),
        (
            "attrregexfilter(buck.package, 'TARGETS.fixture$', root//:plain)",
            {"root//:plain"},
        ),
    ]
    for query, expected in cases:
        result = await buck.cquery(query, "--target-platforms=root//:chosen")
        actual = {line.split(" (", 1)[0] for line in result.stdout.splitlines()}
        assert actual == expected, (query, result.stdout)

    result = await buck.cquery(
        "attrfilter(strings, selected, root//:selected)",
        "--target-platforms=root//:other",
    )
    assert result.stdout == ""
    result = await buck.cquery(
        "attrfilter(strings, unselected, root//:selected)",
        "--target-platforms=root//:other",
    )
    assert result.stdout.startswith("root//:selected (")
