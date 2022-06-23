use std::path::PathBuf;

use async_trait::async_trait;
use events::subscriber::EventSubscriber;
use events::BuckEvent;

pub struct BuildIdWriter {
    path: PathBuf,
}

impl BuildIdWriter {
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }
}

#[async_trait]
impl EventSubscriber for BuildIdWriter {
    async fn handle_command_start(
        &mut self,
        _command: &buck2_data::CommandStart,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(tokio::fs::write(&self.path, event.trace_id.to_string()).await?)
    }
}
