# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_changing_cell_location_bug(buck: Buck) -> None:
    await buck.targets("foo//:", "bar//:")

    # Switch the location of the 2 cells
    (buck.cwd / ".buckconfig").write_text(
        "[cells]\nfoo=bar\nbar=foo\nroot=.\nprelude=.\n"
    )

    # Make sure buck picks up the `CellResolver` updates
    await buck.targets("foo//:", "bar//:")

    (buck.cwd / "foo" / "TARGETS.fixture").write_text("fail('error')")

    # FIXME(JakobDegen): The change to the `TARGETS.fixture` file does not get picked up by buck.
    # The cause is that the file watcher always invalidates injected keys computed from `CellPath`s,
    # but the `CellResolver` that it uses to map `ProjectRelativePath`s to `CellPath`s is computed
    # once at daemon startup and never updated. So concretely, the file update above results in the
    # cell path `bar//TARGETS.fixture` being invalidated, which means the targets in `foo//:` are
    # never recomputed.
    #
    # This is just one example, there's a thousand other ways that you can change the `CellResolver`
    # to create similar bugs.
    await buck.targets("foo//:", "bar//:")
