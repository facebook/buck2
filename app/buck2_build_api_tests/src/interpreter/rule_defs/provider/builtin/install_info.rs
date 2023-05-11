/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::provider::collection::tester::collection_creator;
use buck2_build_api::interpreter::rule_defs::register_rule_defs;
use buck2_common::result::SharedResult;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use buck2_interpreter_for_build::label::testing::label_creator;
use indoc::indoc;

use crate::interpreter::rule_defs::artifact::testing::artifactory;

fn tester() -> Tester {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(collection_creator);
    tester.additional_globals(artifactory);
    tester.additional_globals(label_creator);
    tester.additional_globals(register_rule_defs);
    tester
}

#[test]
fn install_info_works_as_provider_key() -> SharedResult<()> {
    let content = indoc!(
        r#"
             installer_app = label("//foo:bar[quz]")
             c = create_collection([InstallInfo(installer=installer_app, files={}), DefaultInfo(), RunInfo()])
             def test():
                 assert_eq(True, contains_provider(c, InstallInfo))
             "#
    );
    let mut tester = tester();
    tester.run_starlark_bzl_test(content)
}

#[test]
fn info_validator_succeeds_for_artifacts_without_additional_artifacts() -> SharedResult<()> {
    let content = indoc!(
        r#"
             a1 = source_artifact("foo/bar", "baz.h")
             a2 = bound_artifact("//:dep1", "dir/baz.h")
             installer_app = label("//foo:bar[quz]")
             c = create_collection([InstallInfo(installer=installer_app, files={"a1": a1, "a2": a2}), DefaultInfo(), RunInfo()])
             def test():
                 assert_eq(True, contains_provider(c, InstallInfo))
             "#
    );
    let mut tester = tester();
    tester.run_starlark_bzl_test(content)
}
