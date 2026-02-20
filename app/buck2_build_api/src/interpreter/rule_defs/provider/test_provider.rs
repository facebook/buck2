/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_core::cells::name::CellName;
use buck2_test_api::data::ConfiguredTarget;
use buck2_test_api::data::ExternalRunnerSpec;
use buck2_test_api::data::ExternalRunnerSpecValue;
use buck2_test_api::protocol::TestExecutor;
use futures::future::BoxFuture;
use futures::future::FutureExt;
use itertools::Itertools;

use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::provider::builtin::external_runner_test_info::FrozenExternalRunnerTestInfo;
use crate::interpreter::rule_defs::provider::builtin::external_runner_test_info::TestCommandMember;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollection;

pub trait TestProvider {
    fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor<'_>,
    ) -> buck2_error::Result<()>;

    fn labels(&self) -> Vec<&str>;

    fn dispatch<'exec>(
        &self,
        target: ConfiguredTarget,
        executor: Arc<dyn TestExecutor + 'exec>,
        working_dir_cell: CellName,
    ) -> BoxFuture<'exec, buck2_error::Result<()>>;
}

impl TestProvider for FrozenExternalRunnerTestInfo {
    fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor<'_>,
    ) -> buck2_error::Result<()> {
        FrozenExternalRunnerTestInfo::visit_artifacts(self, visitor)
    }

    fn labels(&self) -> Vec<&str> {
        FrozenExternalRunnerTestInfo::labels(self).collect()
    }

    fn dispatch<'exec>(
        &self,
        target: ConfiguredTarget,
        executor: Arc<dyn TestExecutor + 'exec>,
        working_dir_cell: CellName,
    ) -> BoxFuture<'exec, buck2_error::Result<()>> {
        let mut handle_index = 0;

        let command = self
            .command()
            .map(|c| match c {
                TestCommandMember::Literal(l) => ExternalRunnerSpecValue::Verbatim(l.to_owned()),
                TestCommandMember::Arglike(_) => {
                    // We assign indices to handles, which Tpx can use to reference them later.
                    // We don't count literals in here since Tpx won't use handles to
                    // communicate those (it would just use a literal instead).
                    let handle = ExternalRunnerSpecValue::ArgHandle(handle_index.into());
                    handle_index += 1;
                    handle
                }
            })
            .collect();

        let env = self
            .env()
            .map(|(k, _)| {
                (
                    k.to_owned(),
                    ExternalRunnerSpecValue::EnvHandle(k.to_owned().into()),
                )
            })
            .collect();
        let package_oncall = target.package_oncall.clone();

        let spec = ExternalRunnerSpec {
            target,
            test_type: self.test_type().to_owned(),
            command,
            env,
            labels: self.labels().map(|l| l.to_owned()).collect(),
            contacts: self.contacts().map(|l| l.to_owned()).collect(),
            oncall: self
                .contacts()
                .exactly_one()
                .ok()
                .map(str::to_owned)
                .or(package_oncall),
            working_dir_cell,
        };

        async move { executor.external_runner_spec(spec).await }.boxed()
    }
}

impl dyn TestProvider {
    pub fn from_collection(providers: &FrozenProviderCollection) -> Option<&dyn TestProvider> {
        if let Some(provider) = providers.builtin_provider::<FrozenExternalRunnerTestInfo>() {
            return Some(provider.as_ref());
        }

        None
    }
}
