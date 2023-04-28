/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::target::label::ConfiguredTargetLabel;
use derivative::Derivative;
use tokio::sync::mpsc;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::Mutex;

#[derive(Debug, PartialEq)]
pub struct EnvironmentVariable {
    pub key: String,
    pub value: String,
}

/// Resource represented by a list of environment variable key-value pairs.
#[derive(Debug, PartialEq)]
pub struct LocalResource(pub Vec<EnvironmentVariable>);

/// RAII handle for resource spec, returns spec to the pool on drop.
pub struct LocalResourceHolder {
    // Optionality is only needed so we can move out the spec on drop.
    spec: Option<LocalResource>,
    sender: UnboundedSender<LocalResource>,
}

impl Drop for LocalResourceHolder {
    fn drop(&mut self) {
        let _ignored = self.sender.send(
            self.spec
                .take()
                .expect("Should only be absent in already dropped object."),
        );
    }
}

impl AsRef<LocalResource> for LocalResourceHolder {
    fn as_ref(&self) -> &LocalResource {
        self.spec
            .as_ref()
            .expect("Should only be absent in already dropped object.")
    }
}

/// Blocking resource pool to manage access to prepared local resources.
#[derive(Clone, Derivative)]
#[derivative(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalResourceState {
    // Set of resources of same type should be uniquely identified by configured target label providing `LocalResourceInfo`.
    // This is the assumption for equiality, ordering and hash implementations.
    source_target: ConfiguredTargetLabel,
    #[derivative(
        Hash = "ignore",
        PartialEq = "ignore",
        PartialOrd = "ignore",
        Ord = "ignore"
    )]
    owning_pid: i32,
    #[derivative(
        Hash = "ignore",
        PartialEq = "ignore",
        PartialOrd = "ignore",
        Ord = "ignore"
    )]
    sender: UnboundedSender<LocalResource>,
    #[derivative(
        Hash = "ignore",
        PartialEq = "ignore",
        PartialOrd = "ignore",
        Ord = "ignore"
    )]
    receiver: Arc<Mutex<UnboundedReceiver<LocalResource>>>,
}

impl LocalResourceState {
    pub fn new(
        source_target: ConfiguredTargetLabel,
        owning_pid: i32,
        specs: Vec<LocalResource>,
    ) -> Self {
        let (sender, receiver) = mpsc::unbounded_channel();
        for spec in specs {
            sender.send(spec).expect(
                "Not expected send to fail when channel is not closed and receiver is not dropped.",
            );
        }
        LocalResourceState {
            source_target,
            owning_pid,
            sender,
            receiver: Arc::new(Mutex::new(receiver)),
        }
    }

    /// ID of process which actually is holding a pool of resources.
    /// SIGTERM is sent to this process to free the resources.
    pub fn owning_pid(&self) -> i32 {
        self.owning_pid
    }

    pub async fn acquire_resource(&self) -> LocalResourceHolder {
        let spec = {
            let mut guard = self.receiver.lock().await;
            Some(guard.recv().await.unwrap())
        };
        LocalResourceHolder {
            spec,
            sender: self.sender.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::target::label::ConfiguredTargetLabel;

    use super::EnvironmentVariable;
    use crate::local_resource_state::LocalResource;
    use crate::local_resource_state::LocalResourceState;

    #[tokio::test]
    async fn test_canary() -> anyhow::Result<()> {
        let target =
            ConfiguredTargetLabel::testing_parse("foo//bar:baz", ConfigurationData::testing_new());
        let specs = vec![
            LocalResource(vec![EnvironmentVariable {
                key: "FOO".to_owned(),
                value: "foo".to_owned(),
            }]),
            LocalResource(vec![EnvironmentVariable {
                key: "BAR".to_owned(),
                value: "bar".to_owned(),
            }]),
        ];

        let state = LocalResourceState::new(target, 0, specs);
        let handle = tokio::spawn(async move {
            {
                let _holder1 = state.acquire_resource().await;
                let _holder2 = state.acquire_resource().await;
            }
            for _ in 0..10 {
                let _x = state.acquire_resource().await;
            }
        });
        handle.await?;
        Ok(())
    }
}
