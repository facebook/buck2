/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;

use buck2_common::local_resource_state::EnvironmentVariable;
use buck2_common::local_resource_state::LocalResource;
use buck2_common::local_resource_state::LocalResourceState;
use buck2_core::target::label::ConfiguredTargetLabel;
use indexmap::IndexMap;
use serde::Deserialize;

#[derive(Deserialize)]
/// Represents the JSON schema for output of a local resource command setup.
pub(crate) struct LocalResourcesSetupResult {
    /// Process ID which is holding local resources which were set up.
    /// This process needs to be gracefully terminated in order to release
    /// any system resources related to local resources.
    pub(crate) pid: Option<i32>,
    /// A list of resource entities, each is a mapping from a string alias (e.g. `socket_address`)
    /// to a value which represents resource. String alias is mapped to an environment variable key
    /// (which will be added to a command requiring such resource) using a `LocalResourceInfo` provider.
    pub(crate) resources: Vec<BTreeMap<String, String>>,
}

impl LocalResourcesSetupResult {
    pub(crate) fn into_state(
        self,
        resource_target: ConfiguredTargetLabel,
        provider_env_mapping: &IndexMap<String, String>,
    ) -> anyhow::Result<LocalResourceState> {
        fn make_resource(
            alias_to_value: BTreeMap<String, String>,
            env_var_to_alias: &IndexMap<String, String>,
        ) -> anyhow::Result<LocalResource> {
            let env_vars = env_var_to_alias.iter().map(|(env_var, alias)| {
                let value = alias_to_value.get(alias).ok_or_else(|| anyhow::anyhow!("Missing value for local resource environment variable `{}` with `{}` alias", env_var, alias))?.to_owned();
                Ok(EnvironmentVariable {key: env_var.to_owned(), value})
            }).collect::<Result<_, anyhow::Error>>()?;
            Ok(LocalResource(env_vars))
        }
        let specs = self
            .resources
            .into_iter()
            .map(|res| make_resource(res, provider_env_mapping))
            .collect::<Result<_, anyhow::Error>>()?;

        Ok(LocalResourceState::new(resource_target, self.pid, specs))
    }
}

#[cfg(test)]
mod tests {
    use buck2_common::local_resource_state::EnvironmentVariable;
    use buck2_common::local_resource_state::LocalResource;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::target::label::ConfiguredTargetLabel;
    use indexmap::indexmap;
    use maplit::btreemap;

    use crate::local_resource_api::LocalResourcesSetupResult;

    #[tokio::test]
    async fn test_into_state() -> anyhow::Result<()> {
        let setup_result = LocalResourcesSetupResult {
            pid: Some(42),
            resources: vec![
                btreemap! { "socket_address".to_owned() => "foo".to_owned(), "named_pipe".to_owned() => "bar".to_owned() },
                btreemap! { "socket_address".to_owned() => "baz".to_owned(), "something_else".to_owned() => "boo".to_owned() },
                btreemap! { "socket_address".to_owned() => "qux".to_owned() },
            ],
        };
        let target =
            ConfiguredTargetLabel::testing_parse("foo//bar:baz", ConfigurationData::testing_new());
        let provider_env_mapping = indexmap! {
            "ENV_SOCKET".to_owned() => "socket_address".to_owned(),
        };
        let state = setup_result.into_state(target, &provider_env_mapping)?;
        assert_eq!(state.owning_pid(), Some(42));
        let holder1 = state.acquire_resource().await;
        let holder2 = state.acquire_resource().await;
        let holder3 = state.acquire_resource().await;
        assert_eq!(
            holder1.as_ref(),
            &LocalResource(vec![EnvironmentVariable {
                key: "ENV_SOCKET".to_owned(),
                value: "foo".to_owned()
            },])
        );
        assert_eq!(
            holder2.as_ref(),
            &LocalResource(vec![EnvironmentVariable {
                key: "ENV_SOCKET".to_owned(),
                value: "baz".to_owned()
            }])
        );
        assert_eq!(
            holder3.as_ref(),
            &LocalResource(vec![EnvironmentVariable {
                key: "ENV_SOCKET".to_owned(),
                value: "qux".to_owned()
            }])
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_missing_value() -> anyhow::Result<()> {
        let setup_result = LocalResourcesSetupResult {
            pid: Some(42),
            resources: vec![
                btreemap! { "socket_address".to_owned() => "foo".to_owned() },
                btreemap! { "something_else".to_owned() => "bar".to_owned() },
            ],
        };
        let target =
            ConfiguredTargetLabel::testing_parse("foo//bar:baz", ConfigurationData::testing_new());
        let provider_env_mapping = indexmap! {
            "ENV_SOCKET".to_owned() => "socket_address".to_owned(),
        };
        let result = setup_result.into_state(target, &provider_env_mapping);
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("Missing value for local resource environment variable `ENV_SOCKET` with `socket_address` alias"));
        Ok(())
    }
}
