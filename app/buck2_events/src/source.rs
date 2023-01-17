/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;

use crate::Event;
use crate::EventSource;

/// A simple EventSource backed by an unbounded channel.
pub struct ChannelEventSource(crossbeam_channel::Receiver<Event>);

impl ChannelEventSource {
    pub fn new(recv: crossbeam_channel::Receiver<Event>) -> ChannelEventSource {
        ChannelEventSource(recv)
    }
}

#[async_trait]
impl EventSource for ChannelEventSource {
    fn receive(&mut self) -> Option<Event> {
        self.0.recv().ok()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::time::SystemTime;

    use buck2_data::buck_event::Data::SpanStart;
    use buck2_data::span_start_event::Data::Command;
    use buck2_data::CommandStart;
    use buck2_data::SpanStartEvent;

    use super::ChannelEventSource;
    use crate::sink::channel::ChannelEventSink;
    use crate::BuckEvent;
    use crate::EventSink;
    use crate::EventSource;
    use crate::TraceId;

    #[tokio::test]
    async fn receive_smoke() {
        let (send, recv) = crossbeam_channel::unbounded();
        let sink = ChannelEventSink::new(send);
        let mut source = ChannelEventSource::new(recv);
        sink.send(BuckEvent::new(
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
        ));
        let event = source.receive().unwrap().unpack_buck().unwrap().clone();
        assert!(matches!(
            event.data(),
            SpanStart(SpanStartEvent {
                data: Some(Command(CommandStart { .. }))
            })
        ));
    }
}
