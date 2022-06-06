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
use gazebo::any::ProvidesStaticType;
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
#[derive(Clone, Coerce, Debug, Freeze, Trace, ProvidesStaticType)]
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
        installer: ValueOf<'v, &'v RunInfo<'v>>,
        files: ValueOf<'v, SmallMap<&'v str, Value<'v>>>,
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

    use crate::interpreter::{rule_defs::provider::tester::collection_creator, testing::Tester};

    #[test]
    fn install_info_works_as_provider_key() -> SharedResult<()> {
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
