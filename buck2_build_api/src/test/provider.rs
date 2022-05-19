/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use futures::future::{BoxFuture, FutureExt};
use test_api::{
    data::{ConfiguredTarget, ExternalRunnerSpec, ExternalRunnerSpecValue},
    protocol::TestExecutor,
};

use crate::interpreter::rule_defs::{
    cmd_args::{AbsCommandLineBuilder, CommandLineArtifactVisitor},
    provider::{
        external_runner_test_info::{
            ExternalRunnerTestInfoCallable, FrozenExternalRunnerTestInfo, TestCommandMember,
        },
        FrozenProviderCollection,
    },
};

pub trait TestProvider {
    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()>;

    fn labels(&self) -> Vec<&str>;

    fn dispatch<'exec>(
        &self,
        target: ConfiguredTarget,
        executor: Arc<dyn TestExecutor + 'exec>,
    ) -> BoxFuture<'exec, anyhow::Result<()>>;

    fn create_command<'b>(
        &self,
        builder: &dyn Fn() -> AbsCommandLineBuilder<'b>,
    ) -> anyhow::Result<AbsCommandLineBuilder<'b>>;

    fn create_env<'b>(
        &self,
        builder: &dyn Fn() -> AbsCommandLineBuilder<'b>,
    ) -> anyhow::Result<Vec<(String, AbsCommandLineBuilder<'b>)>>;
}

impl TestProvider for FrozenExternalRunnerTestInfo {
    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        FrozenExternalRunnerTestInfo::visit_artifacts(self, visitor)
    }

    fn labels(&self) -> Vec<&str> {
        FrozenExternalRunnerTestInfo::labels(self).collect()
    }

    fn dispatch<'exec>(
        &self,
        target: ConfiguredTarget,
        executor: Arc<dyn TestExecutor + 'exec>,
    ) -> BoxFuture<'exec, anyhow::Result<()>> {
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

        let spec = ExternalRunnerSpec {
            target,
            test_type: self.test_type().to_owned(),
            command,
            env,
            labels: self.labels().map(|l| l.to_owned()).collect(),
            contacts: self.contacts().map(|l| l.to_owned()).collect(),
        };

        async move { executor.external_runner_spec(spec).await }.boxed()
    }

    fn create_command<'b>(
        &self,
        builder: &dyn Fn() -> AbsCommandLineBuilder<'b>,
    ) -> anyhow::Result<AbsCommandLineBuilder<'b>> {
        let mut ret = builder();
        for member in self.command() {
            member.add_to_command_line(&mut ret)?;
        }
        Ok(ret)
    }

    fn create_env<'b>(
        &self,
        builder: &dyn Fn() -> AbsCommandLineBuilder<'b>,
    ) -> anyhow::Result<Vec<(String, AbsCommandLineBuilder<'b>)>> {
        self.env()
            .map(|(k, v)| {
                Ok((k.to_owned(), {
                    let mut ret = builder();
                    v.add_to_command_line(&mut ret)?;
                    ret
                }))
            })
            .collect()
    }
}

impl dyn TestProvider {
    pub fn from_collection<'a>(
        providers: &'a FrozenProviderCollection,
    ) -> Option<&'a dyn TestProvider> {
        if let Some(provider) =
            providers.get_provider(ExternalRunnerTestInfoCallable::provider_id_t())
        {
            return Some(provider.as_ref());
        }

        None
    }
}
