//! A Sink for forwarding events directly to Scribe.

use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(fbcode_build)]
use fbinit::FacebookInit;
#[cfg(fbcode_build)]
use prost::Message;

#[cfg(fbcode_build)]
use crate::{BuckEvent, ControlEvent, EventSink};

/// Whether or not Scribe logging is enabled for this process. It must be explicitly disabled via `disable()`.
static SCRIBE_ENABLED: AtomicBool = AtomicBool::new(true);

/// ThriftScribeSink is a ScribeSink backed by the Thrift-based client in the `buck2_scribe_client` crate.
#[cfg(fbcode_build)]
pub struct ThriftScribeSink {
    category: String,
    client: scribe_client::ScribeClient,
}

#[cfg(fbcode_build)]
impl ThriftScribeSink {
    /// Creates a new ThriftScribeSink that forwards messages onto the Thrift-backed Scribe client.
    pub fn new(
        fb: FacebookInit,
        category: String,
        buffer_size: usize,
    ) -> anyhow::Result<ThriftScribeSink> {
        let client = scribe_client::ScribeClient::new(fb, buffer_size)?;
        Ok(ThriftScribeSink { category, client })
    }

    pub async fn flush_blocking(&self) {
        self.client.flush_blocking().await;
    }
}

#[cfg(fbcode_build)]
impl EventSink for ThriftScribeSink {
    fn send(&self, event: BuckEvent) {
        if !should_send_event(&event.data) {
            return;
        }

        let message_key = event.trace_id.hash();
        let proto: buck2_data::BuckEvent = event.into();
        let mut buf = Vec::with_capacity(proto.encoded_len());
        proto
            .encode(&mut buf)
            .expect("failed to encode protobuf message");

        // Scribe requires that payloads sent through it be valid strings. Since protobuf serializes to bytes, we
        // re-encode them as base64 here. This is not super ideal, but it does work.
        let b64 = base64::encode(&buf);
        self.client.offer(scribe_client::Message {
            category: self.category.clone(),
            message: b64.as_bytes().to_vec(),
            message_key: Some(message_key),
        });
    }

    fn send_control(&self, _control_event: ControlEvent) {}
}

/// Returns whether this process should actually write to Scribe, even if it is fully supported by the platform and
/// binary.
pub fn is_enabled() -> bool {
    SCRIBE_ENABLED.load(Ordering::Relaxed)
}

/// Disables Scribe logging for this process. Scribe logging must be disabled explicitly on startup, otherwise it is
/// on by default.
pub fn disable() {
    SCRIBE_ENABLED.store(false, Ordering::Relaxed);
}

#[cfg_attr(not(fbcode_build), allow(unused))]
fn should_send_event(d: &buck2_data::buck_event::Data) -> bool {
    use buck2_data::buck_event::Data;

    match d {
        Data::SpanStart(s) => {
            use buck2_data::span_start_event::Data;

            match s.data {
                Some(Data::Command(..)) => true,
                Some(Data::ActionExecution(..)) => true,
                Some(Data::Analysis(..)) => true,
                Some(Data::AnalysisStage(..)) => false,
                Some(Data::FinalMaterialization(..)) => true,
                Some(Data::Load(..)) => true,
                Some(Data::LoadPackage(..)) => true,
                Some(Data::ExecutorStage(..)) => false,
                Some(Data::TestDiscovery(..)) => true,
                Some(Data::TestStart(..)) => true,
                Some(Data::Watchman(..)) => true,
                Some(Data::MatchDepFiles(..)) => false,
                None => false,
            }
        }
        Data::SpanEnd(s) => {
            use buck2_data::span_end_event::Data;

            match s.data {
                Some(Data::Command(..)) => true,
                Some(Data::ActionExecution(..)) => true,
                Some(Data::Analysis(..)) => true,
                Some(Data::AnalysisStage(..)) => false,
                Some(Data::FinalMaterialization(..)) => true,
                Some(Data::Load(..)) => true,
                Some(Data::LoadPackage(..)) => true,
                Some(Data::ExecutorStage(..)) => false,
                Some(Data::TestDiscovery(..)) => true,
                Some(Data::TestEnd(..)) => true,
                Some(Data::SpanCancelled(..)) => false,
                Some(Data::Watchman(..)) => true,
                Some(Data::MatchDepFiles(..)) => false,
                None => false,
            }
        }
        Data::Instant(i) => {
            use buck2_data::instant_event::Data;

            match i.data {
                Some(Data::RawOutput(..)) => false,
                None => false,
                _ => true,
            }
        }
    }
}
