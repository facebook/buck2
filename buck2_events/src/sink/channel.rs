use gazebo::prelude::*;

use crate::BuckEvent;
use crate::ControlEvent;
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
    fn send(&self, event: BuckEvent) {
        let _ignore = self.0.send(event.into());
    }

    fn send_control(&self, control_event: ControlEvent) {
        if let Err(e) = self.0.send(control_event.into()) {
            panic!("failed to send control event to ChannelEventSink: {}", e);
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
    use crate::EventSink;
    use crate::TraceId;

    #[tokio::test]
    async fn sending_event_smoke() {
        let (send, recv) = crossbeam_channel::unbounded();
        let sink = ChannelEventSink::new(send);
        sink.send(BuckEvent {
            timestamp: SystemTime::now(),
            trace_id: TraceId::new(),
            span_id: None,
            parent_id: None,
            data: SpanStartEvent {
                data: Some(
                    CommandStart {
                        data: None,
                        metadata: HashMap::new(),
                    }
                    .into(),
                ),
            }
            .into(),
        });
        let event = recv.recv().unwrap().unpack_buck().unwrap().clone();
        assert!(matches!(
            event.data,
            SpanStart(SpanStartEvent {
                data: Some(Command(CommandStart { .. }))
            })
        ));
    }
}
