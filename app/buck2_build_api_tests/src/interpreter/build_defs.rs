/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::build_defs::register_provider;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use indoc::indoc;

#[test]
fn test_provider() -> anyhow::Result<()> {
    // TODO: test restricting field names
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_provider);
    tester.run_starlark_test(indoc!(
        r#"
            SomeInfo = provider(fields=["x", "y"])
            SomeOtherInfo = provider(fields={"x": "docs for x", "y": "docs for y"})
            DocInfo = provider(doc="Some docs", fields=["x", "y"])

            def test():
                instance = SomeInfo(x = 2, y = True)
                assert_eq(2, instance.x)
                assert_eq(True, instance.y)
                assert_eq(SomeInfo(x = 2, y = True), instance)

                instance = SomeOtherInfo(x = 2, y = True)
                assert_eq(2, instance.x)
                assert_eq(True, instance.y)
                assert_eq(SomeOtherInfo(x = 2, y = True), instance)

                instance = DocInfo(x = 2, y = True)
                assert_eq(2, instance.x)
                assert_eq(True, instance.y)
                assert_eq(DocInfo(x = 2, y = True), instance)
            "#
    ))?;
    Ok(())
}
