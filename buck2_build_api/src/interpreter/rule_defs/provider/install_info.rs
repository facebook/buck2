/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Deref;

use buck2_build_api_derive::internal_provider;
use gazebo::any::AnyLifetime;
use indexmap::IndexMap;
use starlark::{
    collections::SmallMap,
    environment::GlobalsBuilder,
    values::{dict::*, Coerce, Freeze, Trace, Value, ValueError, ValueOf},
};

use crate::{
    actions::artifact::Artifact,
    interpreter::rule_defs::{artifact::ValueAsArtifactLike, provider::run_info::RunInfo},
};
// Provider that singals a rule is installable

#[internal_provider(install_info_creator)]
#[derive(Clone, Coerce, Debug, Freeze, Trace, AnyLifetime)]
#[repr(C)]
pub struct InstallInfoGen<V> {
    // RunInfo for the installer
    installer: V,
    // list of files that need to be installed
    // Dict<"String": Artifact>
    files: V,
}

impl FrozenInstallInfo {
    pub fn get_installer(&self) -> &RunInfo {
        RunInfo::from_value(self.installer.to_value()).expect("Value is a RunInfo")
    }

    pub fn get_files(&self) -> anyhow::Result<IndexMap<&str, Artifact>> {
        let mut artifacts: IndexMap<&str, Artifact> = IndexMap::new();
        let files = Dict::from_value(self.files.to_value()).expect("Value is a Dict");
        for (k, v) in files.deref().iter() {
            artifacts.insert(
                k.unpack_str().expect("should be a string"),
                v.as_artifact()
                    .ok_or_else(|| anyhow::anyhow!("not an artifact"))?
                    .get_bound()?,
            );
        }
        Ok(artifacts)
    }
}

#[starlark_module]
fn install_info_creator(globals: &mut GlobalsBuilder) {
    fn InstallInfo<'v>(
        installer: ValueOf<&RunInfo>,
        files: ValueOf<SmallMap<&str, Value<'v>>>,
    ) -> anyhow::Result<InstallInfo<'v>> {
        for v in files.typed.values() {
            v.as_artifact().ok_or(ValueError::IncorrectParameterType)?;
        }
        let files = files.value;
        Ok(InstallInfo {
            installer: *installer,
            files,
        })
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
            a = source_artifact("foo", "bar.o")
            b = source_artifact("foo/bar", "baz.o")
            c = source_artifact("bar", "baz.o")
            f1 ={}
            f2={"foo": a}
            f3={"c": c,  "b":b}

            ri1=RunInfo()
            ri2=RunInfo(args=["foo.o"])
            ri3=RunInfo(args=["bar/foo.o","-some_option"])

            frozen_ii1=InstallInfo(installer=ri1, files={})
            frozen_ii2=InstallInfo(installer=ri2,files=f2)
            frozen_ii3=InstallInfo(installer=ri3,files=f3)

            def test():
                ri=RunInfo()
                assert_eq("RunInfo(args=cmd_args())", repr(frozen_ii1.installer))
                assert_eq({}, frozen_ii1.files)
                assert_eq([], get_args(frozen_ii1.installer))
                assert_eq(["foo.o"], get_args(frozen_ii2.installer))
                assert_eq(["foo"], frozen_ii2.files.keys())

                assert_eq(["bar/foo.o", "-some_option"], get_args(frozen_ii3.installer))
                assert_eq("<source bar/baz.o>",repr(frozen_ii3.files["c"]))
            "#
        );
        tester.run_starlark_bzl_test(content)
    }

    #[test]
    fn run_info_validates_types() {
        let content_bad_args1 = indoc!(
            r#"
            def test():
                df = DefaultInfo()
                InstallInfo(installer=df,files={})
            "#
        );
        run_starlark_bzl_test_expecting_error(content_bad_args1, "Type of parameter");

        let content_bad_args2 = indoc!(
            r#"
            def test():
                InstallInfo(installer=RunInfo(),files=[])
            "#
        );
        run_starlark_bzl_test_expecting_error(content_bad_args2, "Type of parameter");

        let content_bad_args3 = indoc!(
            r#"
            def test():
                InstallInfo(installer=RunInfo(),files={"foo":"foo.o"})
            "#
        );
        run_starlark_bzl_test_expecting_error(content_bad_args3, "Type of parameter");
    }

    #[test]
    fn test_info_works_as_provider_key() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(Arc::new(collection_creator));

        let content = indoc!(
            r#"
            c = create_collection([InstallInfo(installer=RunInfo(), files={}), DefaultInfo(), RunInfo()])
            def test():
                assert_eq(True, contains_provider(c, InstallInfo))
            "#
        );

        tester.run_starlark_bzl_test(content)
    }
}
