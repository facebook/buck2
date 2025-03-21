/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::time::Duration;
use std::time::Instant;

use futures::stream::FuturesUnordered;
use futures::StreamExt;

use crate::subscribers::observer::ErrorObserver;
use crate::subscribers::subscriber::EventSubscriber;

#[derive(Default)]
pub struct EventSubscribers {
    subscribers: Vec<Box<dyn EventSubscriber>>,
}

impl EventSubscribers {
    pub fn new(subscribers: Vec<Box<dyn EventSubscriber>>) -> EventSubscribers {
        EventSubscribers { subscribers }
    }

    /// Helper method to abstract the process of applying an `EventSubscriber` method to all of the subscribers.
    /// Quits on the first error encountered.
    pub(crate) async fn for_each_subscriber<'b, Fut>(
        &'b mut self,
        f: impl FnMut(&'b mut Box<dyn EventSubscriber>) -> Fut,
    ) -> buck2_error::Result<()>
    where
        Fut: Future<Output = buck2_error::Result<()>> + 'b,
    {
        let mut futures: FuturesUnordered<_> = self.subscribers.iter_mut().map(f).collect();
        while let Some(res) = futures.next().await {
            res?;
        }
        Ok(())
    }

    pub(crate) async fn handle_exit(&mut self) -> buck2_error::Result<()> {
        let mut r = Ok(());
        for subscriber in &mut self.subscribers {
            // Exit all subscribers, do not stop on first one.
            let subscriber_err = subscriber.exit().await;
            if r.is_ok() {
                // Keep first error.
                r = subscriber_err;
            }
        }
        r
    }

    pub(crate) fn handle_daemon_connection_failure(&mut self, error: &buck2_error::Error) {
        for subscriber in &mut self.subscribers {
            subscriber.handle_daemon_connection_failure(error);
        }
    }

    pub(crate) fn handle_daemon_started(&mut self, reason: buck2_data::DaemonWasStartedReason) {
        for subscriber in &mut self.subscribers {
            subscriber.handle_daemon_started(reason);
        }
    }

    pub(crate) fn handle_should_restart(&mut self) {
        for subscriber in &mut self.subscribers {
            subscriber.handle_should_restart();
        }
    }

    pub(crate) fn error_observers(&self) -> impl Iterator<Item = &dyn ErrorObserver> {
        self.subscribers
            .iter()
            .filter_map(|s| s.as_error_observer())
    }

    pub(crate) async fn eprintln(&mut self, message: &str) -> buck2_error::Result<()> {
        self.for_each_subscriber(|s| {
            // TODO(nga): this is not a tailer.
            s.handle_tailer_stderr(message)
        })
        .await
    }

    pub(crate) async fn finalize(&mut self) -> Vec<String> {
        let mut errors = Vec::new();
        for subscriber in &mut self.subscribers {
            let start = Instant::now();
            let res = subscriber.finalize().await;
            let elapsed = start.elapsed();
            if elapsed > Duration::from_millis(1000) {
                tracing::warn!("Finalizing \'{}\' took {:?}", subscriber.name(), elapsed);
            } else {
                tracing::info!("Finalizing \'{}\' took {:?}", subscriber.name(), elapsed);
            };

            if let Err(e) = res {
                errors.push(format!(
                    "{:?}",
                    e.context(format!("\'{}\' failed to finalize", subscriber.name()))
                ));
            }
        }
        errors
    }
}
