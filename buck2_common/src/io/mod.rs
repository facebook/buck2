#[cfg(all(unix, feature = "eden_io"))]
pub mod eden;
pub mod fs;

use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePathBuf;
use gazebo::cmp::PartialEqAny;

use crate::file_ops::PathMetadataOrRedirection;
use crate::file_ops::SimpleDirEntry;
use crate::legacy_configs::LegacyBuckConfig;

#[async_trait]
pub trait IoProvider: Send + Sync {
    async fn read_file(&self, path: ProjectRelativePathBuf) -> anyhow::Result<String>;

    async fn read_dir(&self, path: ProjectRelativePathBuf) -> anyhow::Result<Vec<SimpleDirEntry>>;

    async fn read_path_metadata_if_exists(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<Option<PathMetadataOrRedirection<ProjectRelativePathBuf>>>;

    /// Request that this I/O provider be up to date with whatever I/O operations the user might
    /// have done until this point.
    async fn settle(&self) -> anyhow::Result<()>;

    fn name(&self) -> &'static str;

    fn eq_token(&self) -> PartialEqAny<'_>;

    fn fs(&self) -> &Arc<ProjectFilesystem>;
}

impl PartialEq for dyn IoProvider {
    fn eq(&self, other: &dyn IoProvider) -> bool {
        self.eq_token() == other.eq_token()
    }
}

pub async fn create_io_provider(
    fb: fbinit::FacebookInit,
    project_fs: Arc<ProjectFilesystem>,
    root_config: Option<&LegacyBuckConfig>,
) -> anyhow::Result<Arc<dyn IoProvider>> {
    #[cfg(all(unix, feature = "eden_io"))]
    {
        let allow_eden_io_by_default = cfg!(target_os = "macos");

        let allow_eden_io = root_config
            .and_then(|c| c.parse::<bool>("buck2", "allow_eden_io").transpose())
            .transpose()?
            .unwrap_or(allow_eden_io_by_default);

        if allow_eden_io {
            if let Some(eden) = eden::EdenIoProvider::new(fb, &project_fs).await? {
                return Ok(Arc::new(eden));
            }
        }
    }

    let _allow_unused = fb;
    let _allow_unused = root_config;

    Ok(Arc::new(fs::FsIoProvider::new(project_fs)))
}
