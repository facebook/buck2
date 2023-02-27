/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(unused)]

use std::collections::HashMap;

use buck2_core::fs::project_rel_path::ProjectRelativePath;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use tokio::sync::oneshot::Sender;

use crate::materializers::deferred::DeferredMaterializerCommandProcessor;
use crate::materializers::deferred::IoHandler;
use crate::materializers::deferred::MaterializerCommand;
use crate::materializers::deferred::MaterializerSender;

/// Subscriptions allow clients to request eager materialization of specific paths as well as
/// notifications when those paths are materialized.
pub(super) struct MaterializerSubscriptions {
    index: SubscriptionIndex,
    active: HashMap<SubscriptionIndex, ()>,
}

impl MaterializerSubscriptions {
    pub fn new() -> Self {
        Self {
            index: SubscriptionIndex(0),
            active: HashMap::new(),
        }
    }

    /// Return whether a given path should be materialized eagerly.
    pub fn should_materialize_eagerly(&self, path: &ProjectRelativePath) -> bool {
        false
    }

    /// Notify this subscription that a given path has been materialized.
    pub fn on_materialization_finished(&self, path: &ProjectRelativePath) {}

    #[cfg(test)]
    pub(super) fn has_subscription<T>(&self, handle: &SubscriptionHandle<T>) -> bool
    where
        T: 'static,
    {
        self.active.contains_key(&handle.index)
    }

    #[cfg(test)]
    pub(super) fn has_any_subscriptions(&self) -> bool {
        !self.active.is_empty()
    }
}

/// A index uniquely identifying a given Subscription.
#[derive(
    Eq, PartialEq, Copy, Clone, Dupe, Debug, Ord, PartialOrd, Display, Hash
)]
pub(super) struct SubscriptionIndex(u64);

impl SubscriptionIndex {
    /// Increment the current version, return the previous  vlaue
    fn next(&mut self) -> Self {
        let ret = SubscriptionIndex(self.0);
        self.0 += 1;
        ret
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(super) enum MaterializerSubscriptionOperation<T: 'static> {
    /// Create a new subscription and return the handle.
    Create {
        #[derivative(Debug = "ignore")]
        sender: Sender<SubscriptionHandle<T>>,
    },
    /// Notify the materializer that this subscription is no longer needed.
    Destroy { index: SubscriptionIndex },
}

impl<T> MaterializerSubscriptionOperation<T>
where
    T: IoHandler + 'static,
{
    pub(super) fn execute(self, dm: &mut DeferredMaterializerCommandProcessor<T>) {
        match self {
            Self::Create { sender } => {
                let subscriptions = &mut dm.subscriptions;
                let index = dm.subscriptions.index.next();
                dm.subscriptions.active.insert(index, ());
                let _ignored = sender.send(SubscriptionHandle {
                    index,
                    command_sender: dm.command_sender.dupe(),
                });
            }
            Self::Destroy { index } => {
                dm.subscriptions.active.remove(&index);
            }
        }
    }
}

/// The handle we return to clients for a subscription. They can use this to add more things to
/// their subscription and listen to events. When dropped, the subscription will be cancelled.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(super) struct SubscriptionHandle<T: 'static> {
    index: SubscriptionIndex,
    #[derivative(Debug = "ignore")]
    command_sender: MaterializerSender<T>,
}

impl<T: 'static> Drop for SubscriptionHandle<T> {
    fn drop(&mut self) {
        let _ignored = self.command_sender.send(MaterializerCommand::Subscription(
            MaterializerSubscriptionOperation::Destroy { index: self.index },
        ));
    }
}
