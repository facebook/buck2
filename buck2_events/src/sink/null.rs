use crate::BuckEvent;
use crate::ControlEvent;
use crate::EventSink;

/// A null EventSink that discards all messages sent to it.
pub struct NullEventSink;

impl NullEventSink {
    pub fn new() -> NullEventSink {
        NullEventSink
    }
}

impl Default for NullEventSink {
    fn default() -> Self {
        NullEventSink::new()
    }
}

impl EventSink for NullEventSink {
    fn send(&self, _: BuckEvent) {}

    fn send_control(&self, _: ControlEvent) {}
}
