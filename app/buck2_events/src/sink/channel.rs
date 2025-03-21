/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::soft_error;
use buck2_error::conversion::from_any_with_tag;
use dupe::Dupe;

use crate::Event;
use crate::EventSink;

/// An EventSink implementation that pushes events onto an unbounded channel, to be consumed by receivers on said
/// channel.
#[derive(Clone)]
pub struct ChannelEventSink(crossbeam_channel::Sender<Event>);

impl ChannelEventSink {
    pub fn new(send: crossbeam_channel::Sender<Event>) -> ChannelEventSink {
        ChannelEventSink(send)
    }
}

impl EventSink for ChannelEventSink {
    fn send(&self, event: Event) {
        let should_panic = match &event {
            // Sometimes daemon tries to send events after the clients disconnects
            Event::Buck(..) => false,
            Event::PartialResult(..) => true,
            Event::CommandResult(..) => true,
        };
        if let Err(e) = self.0.send(event) {
            if should_panic {
                // TODO iguridi: this panic was here before. We probably should just ignore these errors
                // but first, let's check how often this happens.
                let _res = soft_error!("event_sink_send_panic", from_any_with_tag(e.clone(), buck2_error::ErrorTag::Tier0), quiet: true);
                panic!("failed to send control event to ChannelEventSink: {}", e);
            }
        }
    }
}

// Justification: UnboundedSender is backed by a
// [`Tx`](https://github.com/tokio-rs/tokio/blob/tokio-0.2.25/tokio/src/sync/mpsc/chan.rs#L224-L232),
// whose Clone implementation bumps a few atomics and clones an Arc.
impl Dupe for ChannelEventSink {}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::time::SystemTime;

    use buck2_data::buck_event::Data::SpanStart;
    use buck2_data::span_start_event::Data::Command;
    use buck2_data::CommandStart;
    use buck2_data::SpanStartEvent;

    use super::ChannelEventSink;
    use crate::BuckEvent;
    use crate::Event;
    use crate::EventSink;
    use crate::TraceId;

    #[tokio::test]
    async fn sending_event_smoke() {
        let (send, recv) = crossbeam_channel::unbounded();
        let sink = ChannelEventSink::new(send);
        sink.send(Event::Buck(BuckEvent::new(
            SystemTime::now(),
            TraceId::new(),
            None,
            None,
            SpanStartEvent {
                data: Some(
                    CommandStart {
                        data: None,
                        metadata: HashMap::new(),
                    }
                    .into(),
                ),
            }
            .into(),
        )));
        let event = recv.recv().unwrap().unpack_buck().unwrap().clone();
        assert!(matches!(
            event.data(),
            SpanStart(SpanStartEvent {
                data: Some(Command(CommandStart { .. }))
            })
        ));
    }
}
