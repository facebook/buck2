//! Implementations of `[crate::EventSink]` that are useful in different situations. Buck2 primarily uses the `channel`
//! sink during normal operation.
pub(crate) mod channel;
pub(crate) mod null;
pub mod scribe;
pub mod tee;
