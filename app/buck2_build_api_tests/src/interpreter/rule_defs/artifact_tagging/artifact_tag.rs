/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use dupe::Dupe;
use indoc::indoc;

use crate::interpreter::rule_defs::artifact_tagging::testing::artifact_tag_factory;

#[test]
fn test_artifact_tag_eq() {
    let t1 = ArtifactTag::new();
    let t2 = ArtifactTag::new();

    assert_eq!(t1, t1.dupe());
    assert_ne!(t1, t2);
}

#[test]
fn test_artifact_tag_starlark_eq() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(artifact_tag_factory);

    tester.run_starlark_bzl_test(indoc!(
        r#"
        def test():
            t1 = make_tag()
            t2 = make_tag()

            assert_eq(t1, t1)
            assert_ne(t1, t2)
        "#
    ))?;

    Ok(())
}
