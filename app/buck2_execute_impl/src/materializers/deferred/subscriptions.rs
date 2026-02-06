/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![allow(unused)]

use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::materialize::materializer::DeferredMaterializerSubscription;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use futures::stream::Stream;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::mpsc::unbounded_channel;
use tokio::sync::oneshot::Sender;

use crate::materializers::deferred::DeferredMaterializerCommandProcessor;
use crate::materializers::deferred::IoHandler;
use crate::materializers::deferred::MaterializerCommand;
use crate::materializers::deferred::MaterializerSender;

/// Subscriptions allow clients to request eager materialization of specific paths as well as
/// notifications when those paths are materialized.
pub(super) struct MaterializerSubscriptions {
    index: SubscriptionIndex,
    active: HashMap<SubscriptionIndex, SubscriptionData>,
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
        for sub in self.active.values() {
            if sub.paths.contains(path) {
                return true;
            }
        }

        false
    }

    /// Notify this subscription that a given path has been materialized.
    pub fn on_materialization_finished(&self, path: &ProjectRelativePath) {
        for sub in self.active.values() {
            if sub.paths.contains(path) {
                sub.sender.send(path.to_owned());
            }
        }
    }

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

    pub(super) fn list_subscribed_paths(&self) -> impl Iterator<Item = &ProjectRelativePath> {
        let mut seen = HashSet::new();

        self.active
            .values()
            .flat_map(|v| v.paths.iter())
            .map(|p| p.as_ref())
            .filter(move |p| seen.insert(*p))
    }
}

struct SubscriptionData {
    paths: HashSet<ProjectRelativePathBuf>,
    sender: UnboundedSender<ProjectRelativePathBuf>,
}

impl SubscriptionData {
    fn new(sender: UnboundedSender<ProjectRelativePathBuf>) -> Self {
        Self {
            paths: HashSet::new(),
            sender,
        }
    }
}

/// A index uniquely identifying a given Subscription.
#[derive(
    Eq, PartialEq, Copy, Clone, Dupe, Debug, Ord, PartialOrd, Display, Hash
)]
pub(super) struct SubscriptionIndex(u64);

impl SubscriptionIndex {
    /// Increment the current version, return the previous  value
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

    /// Ask the materializer to send new notifications for the following paths.
    Subscribe {
        index: SubscriptionIndex,
        paths: Vec<ProjectRelativePathBuf>,
    },

    /// Ask the materializer to stop sending notifications for the following paths.
    Unsubscribe {
        index: SubscriptionIndex,
        paths: Vec<ProjectRelativePathBuf>,
    },
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
                let (notification_sender, notification_receiver) = unbounded_channel();
                dm.subscriptions
                    .active
                    .insert(index, SubscriptionData::new(notification_sender));
                let _ignored = sender.send(SubscriptionHandle {
                    index,
                    command_sender: dm.command_sender.dupe(),
                    receiver: notification_receiver,
                });
            }
            Self::Destroy { index } => {
                dm.subscriptions.active.remove(&index);
            }
            Self::Subscribe { index, paths } => {
                let mut paths_to_report = Vec::new();

                for path in &paths {
                    if dm.is_path_materialized(path) {
                        paths_to_report.push(path.to_owned());
                    } else {
                        dm.materialize_artifact(path, EventDispatcher::null());
                    }
                }

                // Messages are processed in order and handles delete themselves when they are
                // dropped so it's not possible for us to receive this message without the
                // underlying subscription existing.
                let subscription = dm
                    .subscriptions
                    .active
                    .get_mut(&index)
                    .ok_or_else(|| internal_error!("Invalid subscription: {index}"))
                    .unwrap();

                for path in paths_to_report {
                    subscription.sender.send(path);
                }

                subscription.paths.extend(paths);
            }
            Self::Unsubscribe { index, paths } => {
                // Same as above, we guarantee that subscriptions cannot send messages after
                // they're deleted.
                let subscription = dm
                    .subscriptions
                    .active
                    .get_mut(&index)
                    .ok_or_else(|| internal_error!("Invalid subscription: {index}"))
                    .unwrap();

                for path in &paths {
                    subscription.paths.remove(path);
                }
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
    command_sender: Arc<MaterializerSender<T>>,
    /// Channel to send back notifications.
    #[derivative(Debug = "ignore")]
    receiver: UnboundedReceiver<ProjectRelativePathBuf>,
}

impl<T: 'static> SubscriptionHandle<T> {
    #[cfg(test)]
    pub fn receiver(&mut self) -> &mut UnboundedReceiver<ProjectRelativePathBuf> {
        &mut self.receiver
    }
}

#[async_trait]
impl<T: 'static> DeferredMaterializerSubscription for SubscriptionHandle<T> {
    fn subscribe_to_paths(&mut self, paths: Vec<ProjectRelativePathBuf>) {
        self.command_sender.send(MaterializerCommand::Subscription(
            MaterializerSubscriptionOperation::Subscribe {
                index: self.index,
                paths,
            },
        ));
    }

    fn unsubscribe_from_paths(&mut self, paths: Vec<ProjectRelativePathBuf>) {
        self.command_sender.send(MaterializerCommand::Subscription(
            MaterializerSubscriptionOperation::Unsubscribe {
                index: self.index,
                paths,
            },
        ));
    }

    async fn next_materialization(&mut self) -> Option<ProjectRelativePathBuf> {
        self.receiver.recv().await
    }
}

impl<T: 'static> Drop for SubscriptionHandle<T> {
    fn drop(&mut self) {
        let _ignored = self.command_sender.send(MaterializerCommand::Subscription(
            MaterializerSubscriptionOperation::Destroy { index: self.index },
        ));
    }
}
