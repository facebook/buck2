/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use buck2_build_api_derive::internal_provider;
use gazebo::{any::ProvidesStaticType, coerce::Coerce};
use starlark::{
    environment::GlobalsBuilder,
    eval::Evaluator,
    values::{list::FrozenList, Freeze, Trace, Value, ValueLike},
};

use crate::interpreter::rule_defs::cmd_args::{
    CommandLineArgLike, CommandLineArtifactVisitor, CommandLineBuilder, StarlarkCommandLine,
    ValueAsCommandLineLike, WriteToFileMacroVisitor,
};

/// Provider that signals that a rule is runnable
#[internal_provider(run_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType)]
#[repr(transparent)]
pub struct RunInfoGen<V> {
    /// The command to run, stored as CommandLine
    args: V,
}

#[starlark_module]
fn run_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(type = "RunInfo")]
    fn RunInfo<'v>(
        // Named _args so it doesn't clash with *args
        #[starlark(default = FrozenList::empty())] _args: Value<'v>,
        eval: &mut Evaluator,
    ) -> anyhow::Result<RunInfo<'v>> {
        let heap = eval.heap();
        let valid_args = StarlarkCommandLine::try_from_value(_args)?;
        Ok(RunInfo {
            args: heap.alloc(valid_args),
        })
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for RunInfoGen<V> {
    fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        self.args
            .to_value()
            .as_command_line()
            .expect("a command line from construction")
            .add_to_command_line(cli)?;
        Ok(())
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        self.args
            .to_value()
            .as_command_line()
            .expect("a command line from construction")
            .visit_artifacts(visitor)?;
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        self.args
            .to_value()
            .as_command_line()
            .expect("a command line from construction")
            .contains_arg_attr()
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_core::result::SharedResult;
    use indoc::indoc;

    use crate::interpreter::{
        rule_defs::{
            artifact::testing::artifactory, cmd_args::tester::command_line_stringifier,
            provider::tester::collection_creator,
        },
        testing::{run_starlark_bzl_test_expecting_error, Tester},
    };

    #[test]
    fn run_info_stringifies() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(Arc::new(|globals| {
            command_line_stringifier(globals);
            artifactory(globals);
        }));
        let content = indoc!(
            r#"
            a = source_artifact("foo/bar", "baz.h")
            b = source_artifact("foo/bar", "baz.cpp")
            c = source_artifact("foo/bar", "baz-inl.h")
            cli = cmd_args().add(a).add(b)
            frozen_ri1 = RunInfo()
            frozen_ri2 = RunInfo(args=["1", "2", cli, a])
            frozen_ri3 = RunInfo(args="1")
            frozen_ri4 = RunInfo(args=cli)
            frozen_ri5 = RunInfo(args=[frozen_ri2, "some_string"])

            cli = cli.copy()
            cli.add(c)

            def test():
                ri1 = RunInfo()
                ri2 = RunInfo(args=["1", "2", cli, a])
                ri3 = RunInfo(args="1")
                ri4 = RunInfo(args=cli)
                ri5 = RunInfo(args=[ri2, "some_string"])
                ri6 = RunInfo(args=[frozen_ri2, "some_string"])

                baz_h = "foo/bar/baz.h"
                baz_cpp = "foo/bar/baz.cpp"
                baz_inl = "foo/bar/baz-inl.h"

                assert_eq([], get_args(frozen_ri1))

                assert_eq(
                    ["1","2", baz_h, baz_cpp, baz_h],
                    get_args(frozen_ri2)
                )

                assert_eq(["1"], get_args(frozen_ri3))

                assert_eq([baz_h, baz_cpp], get_args(frozen_ri4))

                assert_eq(
                    ["1","2", baz_h, baz_cpp, baz_h, "some_string"],
                    get_args(frozen_ri5)
                )

                assert_eq([], get_args(ri1))

                assert_eq(
                    ["1","2", baz_h, baz_cpp, baz_inl, baz_h],
                    get_args(ri2)
                )
                assert_eq(["1"], get_args(ri3))
                assert_eq([baz_h, baz_cpp, baz_inl], get_args(ri4))
                assert_eq(
                    ["1","2", baz_h, baz_cpp, baz_inl, baz_h, "some_string"],
                    get_args(ri5)
                )
                assert_eq(
                    ["1","2", baz_h, baz_cpp, baz_h, "some_string"],
                    get_args(ri6)
                )
            "#
        );
        tester.run_starlark_bzl_test(content)
    }

    #[test]
    fn run_info_validates_types() {
        let content_bad_args1 = indoc!(
            r#"
            def test():
                RunInfo(args=False)
            "#
        );
        run_starlark_bzl_test_expecting_error(content_bad_args1, "expected command line item");

        let content_bad_args2 = indoc!(
            r#"
            def test():
                RunInfo(args={})
            "#
        );
        run_starlark_bzl_test_expecting_error(content_bad_args2, "expected command line item");
    }

    #[test]
    fn run_info_works_as_provider_key() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(Arc::new(collection_creator));

        let content = indoc!(
            r#"
            c = create_collection([RunInfo(args=["1","2","3"]), DefaultInfo()])
            def test():
                assert_eq(True, contains_provider(c, RunInfo))
            "#
        );

        tester.run_starlark_bzl_test(content)
    }
}
