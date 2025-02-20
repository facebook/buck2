/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(once_cell_try)]

mod producer;

use std::sync::Arc;
use std::sync::OnceLock;
use std::time::Duration;

use fbinit::FacebookInit;
pub use producer::Message;
use tokio::runtime::Builder;

use crate::producer::ProducerCounters;
pub use crate::producer::ScribeConfig;
use crate::producer::ScribeProducer;

static PRODUCER: OnceLock<Arc<ScribeProducer>> = OnceLock::new();

/// Initializes the Scribe producer that backs all Scribe clients. Returns an error if a connection can't be
/// established to a remote Scribe daemon process.
fn initialize(fb: FacebookInit, config: ScribeConfig) -> anyhow::Result<&'static ScribeProducer> {
    Ok(&**PRODUCER.get_or_try_init(|| -> anyhow::Result<_> {
        let producer = Arc::new(ScribeProducer::new(fb, config)?);

        // Instead of relying on any existing runtimes, we bootstrap a new tokio runtime bound to a single thread that
        // we spawn here. Running on the same runtime as the rest of the program runs the risk of the producer loop not
        // getting polled in a timely fashion, which leads directly to the message queue filling up and messages getting
        // dropped.
        //
        // We need a separate runtime for now because loading and analysis do large amounts of blocking work on Tokio
        // runtime threads.
        std::thread::Builder::new()
            .name("scribe-producer".to_owned())
            .spawn({
                let producer = producer.clone();
                move || {
                    let runtime = Builder::new_current_thread().enable_all().build().unwrap();
                    runtime.block_on(producer_loop(&producer));
                }
            })?;

        Ok(producer)
    })?)
}

/// Task that drives the producer to regularly drain its queue.
async fn producer_loop(producer: &ScribeProducer) {
    const SLEEP_INTERVAL: Duration = Duration::from_millis(500);

    loop {
        let _res = producer.run_once().await;
        tokio::time::sleep(SLEEP_INTERVAL).await;
    }
}

/// A Scribe client that sends messages to Scribe via `offer`.
pub struct ScribeClient {
    scribe_producer: &'static ScribeProducer,
}

impl ScribeClient {
    pub fn new(fb: FacebookInit, config: ScribeConfig) -> anyhow::Result<ScribeClient> {
        let scribe_producer = initialize(fb, config)?;
        Ok(ScribeClient { scribe_producer })
    }

    pub fn export_counters(&self) -> ProducerCounters {
        self.scribe_producer.export_counters()
    }

    /// Offers a single message to Scribe. Does not block.
    pub fn offer(&self, message: Message) {
        self.scribe_producer.offer(message);
    }

    /// Sends all messages in `messages` now (bypass internal message queue.)
    pub async fn send_messages_now(&self, messages: Vec<Message>) -> anyhow::Result<()> {
        self.scribe_producer
            .send_messages_now(messages)
            .await
            .map_err(|e| e.into())
    }
}
