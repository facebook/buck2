# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

# FIXME(JakobDegen): These tests should be isolated and moved into `tests/isolated/audit`

import json
from pathlib import Path
from typing import Iterable, Set

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


def _classpath_jars(classpaths: Iterable[str]) -> Set[str]:
    return {Path(p).name for p in classpaths}


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_audit_classpath(buck: Buck) -> None:
    result = await buck.audit(
        "classpath", "fbsource//fbandroid/buck2/tests/good/classpath:top"
    )
    classpath_jars = _classpath_jars(result.stdout.splitlines())
    assert classpath_jars == {
        "top.jar",
        "direct_dep.jar",
        "mid_test.jar",
        "transitive_lib.jar",
    }


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_audit_classpath_binary(buck: Buck) -> None:
    result = await buck.audit(
        "classpath", "fbsource//fbandroid/buck2/tests/good/classpath:apk"
    )
    classpath_jars = _classpath_jars(result.stdout.splitlines())
    assert classpath_jars == {
        "dep_of_android_resource.jar",
        "ids_r_dot_java.jar",
        "top.jar",
        "direct_dep.jar",
        "transitive_lib.jar",
        "mid_test.jar",
        "lib_with_resource_only.jar",
    }


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_audit_classpath_json(buck: Buck) -> None:
    top = "fbsource//fbandroid/buck2/tests/good/classpath:top"
    direct_dep = "fbsource//fbandroid/buck2/tests/good/classpath:direct_dep"

    result = await buck.audit("classpath", top, direct_dep, "--json")
    out = json.loads(result.stdout.strip())

    assert len(out.keys()) == 2, f"Found more than 2 targets in {out}"
    assert _classpath_jars(out.get(direct_dep)) == {"direct_dep.jar"}
    assert _classpath_jars(out.get(top)) == {
        "top.jar",
        "direct_dep.jar",
        "mid_test.jar",
        "transitive_lib.jar",
    }
