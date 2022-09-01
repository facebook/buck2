//! The downward api for external processes. This crate defines a trait of downward api that Buck
//! will need to handle as the process runner.

use std::collections::HashMap;

use tracing::Level;

/// The API available to processes that Buck will need to handle
#[async_trait::async_trait]
pub trait DownwardApi {
    /// indicates to print to the console at a specific log level
    async fn console(&self, level: Level, msg: String) -> anyhow::Result<()>;

    /// indicates to log at a specified level
    /// TODO consider if we should have structured log instead of a String message
    async fn log(&self, level: Level, msg: String) -> anyhow::Result<()>;

    /// reports an externally consumable event containing some data that will be untouched by buck
    async fn external(&self, data: HashMap<String, String>) -> anyhow::Result<()>;

    // TODO map the StepEvent and TraceEvents in buckv1 to something. Maybe just a single trace event
}
