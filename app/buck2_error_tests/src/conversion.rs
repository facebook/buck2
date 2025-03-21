/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_error::conversion::from_any_with_tag;
use buck2_util::golden_test_helper::golden_test_template;
use buck2_util::golden_test_helper::trim_rust_backtrace;
use starlark::assert::Assert;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::values::none::NoneType;

fn starlark_conversion_helper() -> starlark::Error {
    fn fail1() -> anyhow::Result<()> {
        Err(anyhow::anyhow!("fail 1"))
    }

    fn fail2() -> anyhow::Result<()> {
        fail1().context("fail 2")
    }

    fn fail3() -> buck2_error::Result<()> {
        fail2()
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Input).context("rust failure"))
    }

    #[starlark_module]
    fn module(builder: &mut GlobalsBuilder) {
        fn rust_failure() -> starlark::Result<NoneType> {
            fail3()?;
            Ok(NoneType)
        }
    }

    let mut a = Assert::new();
    a.globals_add(module);

    a.module(
        "imported",
        r#"
# blank lines to make line numbers bigger and more obvious
#
#
#
#
x = []
def should_fail():
     rust_failure()"#,
    );

    a.fail(
        r#"
load('imported', 'should_fail')
should_fail()"#,
        "rust failure",
    )
}

#[test]
fn test_format_starlark_stacktrace_with_later_context() {
    let e = starlark_conversion_helper();
    let test_context = buck2_error::Error::from(e)
        .context("Adding a context after should still keep backtrace on top");
    golden_test_template(
        "src/golden/test_starlark_callstack_context.golden",
        trim_rust_backtrace(&format!("{:?}", test_context)),
    );
}

#[test]
fn test_starlark_multiple_stacktrace() {
    #[starlark_module]
    fn outer_module(builder: &mut GlobalsBuilder) {
        fn outer_rust_failure() -> starlark::Result<NoneType> {
            let e: buck2_error::Error = starlark_conversion_helper().into();
            Err(e.into())
        }
    }

    let mut a = Assert::new();
    a.globals_add(outer_module);

    a.module(
        "outer_import",
        r#"
x = []
def outer_fail():
     outer_rust_failure()"#,
    );

    let e = a.fail(
        r#"
load('outer_import', 'outer_fail')
outer_fail()"#,
        "rust failure",
    );

    golden_test_template(
        "src/golden/test_starlark_callstack_backtrace.golden",
        trim_rust_backtrace(&format!("{:?}", buck2_error::Error::from(e))),
    );
}

#[test]
fn test_starlark_multiple_stacktrace_with_context_inbetween() {
    #[starlark_module]
    fn outer_module(builder: &mut GlobalsBuilder) {
        fn outer_rust_failure() -> starlark::Result<NoneType> {
            let e: buck2_error::Error = starlark_conversion_helper().into();
            let e = e.context("Adding a context in between backtraces");
            let e = e.context("Error to be displayed in stacktrace");
            Err(e.into())
        }
    }

    let mut a = Assert::new();
    a.globals_add(outer_module);

    a.module(
        "outer_import",
        r#"
x = []
def outer_fail():
     outer_rust_failure()"#,
    );

    let e = a.fail(
        r#"
load('outer_import', 'outer_fail')
outer_fail()"#,
        "Error to be displayed in stacktrace",
    );

    golden_test_template(
        "src/golden/test_starlark_callstack_backtrace_with_context_inbetween.golden",
        trim_rust_backtrace(&format!("{:?}", buck2_error::Error::from(e))),
    );
}
