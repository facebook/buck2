/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use std::sync::Arc;

use anyhow::Context;
use buck2_common::eden::EdenConnectionManager;
use buck2_core::directory::DirectoryEntry;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::process::background_command;
use edenfs::client::EdenService;
use edenfs::CheckoutMode;
use edenfs::EnsureMaterializedParams;
use edenfs::ObjectType;
use edenfs::RemoveRecursivelyParams;
use edenfs::SetPathObjectIdParams;
use fbinit::FacebookInit;
use more_futures::spawn::dropcancel_critical_section;
use serde::Deserialize;
use thiserror::Error;
use tokio::sync::Semaphore;

use crate::actions::directory::ActionDirectoryMember;
use crate::execute::commands::re::manager::ReConnectionManager;
use crate::execute::ArtifactValue;

pub type EdenFsClient = Arc<dyn EdenService + Sync>;

const RE_SYMLINK_PREFIX: &str = "re-symlink";

#[derive(Error, Debug)]
pub enum SetupBuckOutError {
    #[error("Could not read Eden Config")]
    EdenConfigIOError(#[source] std::io::Error),

    #[error("Error during parsing the Eden Config")]
    InvalidEdenConfig(#[source] toml::de::Error),

    #[error("Could not clean buck-out")]
    CleanBuckOutFailed(#[source] std::io::Error),

    #[error("Could not create Eden buck-out")]
    CreateEdenBuckoutOutputFailed(#[source] std::io::Error),

    #[error("Failed to run Eden clone to create buck-out: {0}")]
    FailedToRunEdenClone(String),

    #[error("Could not add {0} to the redirection list of Eden buck-out: {1}")]
    EdenRedirectAddFailed(String, #[source] std::io::Error),

    #[error("Failed to run Eden redirection add path {0} to bind a dir to buck-out: {1}")]
    FailedToRunEdenRedirectionAdd(String, String),
}

#[derive(Deserialize, Debug)]
struct Repository {
    #[serde(rename = "type")]
    repo_type: String,
}

#[derive(Deserialize, Debug)]
struct EdenConfig {
    repository: Repository,
    redirections: Option<BTreeMap<String, String>>,
}

/// Eden has a magic .eden dir for all directories and its config is in .eden/client/config.toml.
/// Read the TOML file and make sure it is recas type mount.
fn read_eden_config(config_path: &Path) -> Result<EdenConfig, SetupBuckOutError> {
    let config_contents =
        fs::read_to_string(config_path).map_err(SetupBuckOutError::EdenConfigIOError)?;
    let config: EdenConfig =
        toml::from_str(&config_contents).map_err(SetupBuckOutError::InvalidEdenConfig)?;
    Ok(config)
}

pub fn is_recas_eden_mount(buck_out: &AbsPathBuf) -> Result<bool, SetupBuckOutError> {
    let config_path = buck_out.join(".eden/client/config.toml");
    if fs::metadata(&config_path).is_err() {
        return Ok(false);
    }
    let config = read_eden_config(&config_path)?;
    Ok(config.repository.repo_type.as_str() == "recas")
}

fn execute_eden_clone(buck_out: &AbsPathBuf) -> Result<(), SetupBuckOutError> {
    // If the buck-out directory exists
    if fs::metadata(buck_out).is_ok() {
        // If it is already an eden-based buck-out, then it's good
        if is_recas_eden_mount(buck_out)? {
            return Ok(());
        }
        // If it is regular FS, remove it first
        fs::remove_dir_all(buck_out).map_err(SetupBuckOutError::CleanBuckOutFailed)?;
    }

    let eden_clone = background_command("eden")
        .arg("clone")
        .arg("")
        .arg(buck_out.as_os_str())
        .arg("--backing-store=recas")
        .arg("--allow-nested-checkout")
        .current_dir("/")
        .output()
        .map_err(SetupBuckOutError::CreateEdenBuckoutOutputFailed)?;

    if !eden_clone.status.success() {
        return Err(SetupBuckOutError::FailedToRunEdenClone(
            String::from_utf8_lossy(&eden_clone.stderr).to_string(),
        ));
    }
    Ok(())
}

fn is_path_redirect(buck_out: &AbsPathBuf, path: &str) -> Result<bool, SetupBuckOutError> {
    let config_path = buck_out.join(".eden/client/config.toml");
    if fs::metadata(&config_path).is_err() {
        return Ok(false);
    }
    let config = read_eden_config(&config_path)?;

    if let Some(redir_config) = config.redirections {
        if let Some(redir_type) = redir_config.get(path) {
            return Ok(redir_type.as_str() == "bind");
        }
    }

    Ok(false)
}

fn execute_eden_redirection_add(
    buck_out: &AbsPathBuf,
    path: &str,
) -> Result<(), SetupBuckOutError> {
    if is_path_redirect(buck_out, path)? {
        return Ok(());
    }
    let eden_direct = background_command("eden")
        .arg("redirect")
        .arg("add")
        .arg(path)
        .arg("bind")
        .current_dir(buck_out.as_os_str())
        .output()
        .map_err(|e| SetupBuckOutError::EdenRedirectAddFailed(String::from(path), e))?;

    if !eden_direct.status.success() {
        return Err(SetupBuckOutError::FailedToRunEdenRedirectionAdd(
            String::from(path),
            String::from_utf8_lossy(&eden_direct.stderr).to_string(),
        ));
    }
    Ok(())
}

fn get_symlink_object_id(target: &str) -> String {
    format!("{}:{}", RE_SYMLINK_PREFIX, target)
}

fn get_object_id_and_type(value: &ArtifactValue) -> (String, ObjectType) {
    match value.entry() {
        DirectoryEntry::Dir(dir) => (dir.fingerprint().to_string(), ObjectType::TREE),
        DirectoryEntry::Leaf(ActionDirectoryMember::File(file)) => {
            let object_type = match file.is_executable {
                true => ObjectType::EXECUTABLE_FILE,
                false => ObjectType::REGULAR_FILE,
            };
            (file.digest.to_string(), object_type)
        }
        DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(symlink)) => (
            get_symlink_object_id(symlink.target().as_str()),
            ObjectType::SYMLINK,
        ),
        DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(external_sym)) => (
            get_symlink_object_id(external_sym.to_path_buf().to_str().unwrap()),
            ObjectType::SYMLINK,
        ),
    }
}

fn setup(buck_out: &AbsPathBuf) -> Result<(), SetupBuckOutError> {
    // Run "eden clone" command to create a brand new buck-out
    execute_eden_clone(buck_out)?;

    // Run eden redirection to exclude write heavy dirs from using EdenFS
    ["log", "tmp", "re_logs"]
        .into_iter()
        .try_for_each(|dir| -> Result<(), SetupBuckOutError> {
            execute_eden_redirection_add(buck_out, dir)?;
            Ok(())
        })?;

    Ok(())
}

pub struct EdenBuckOut {
    /// Relative path of buck_out, i.e "buck-out/v2"
    buck_out_path: ProjectRelativePathBuf,
    connection_manager: EdenConnectionManager,
    re_client_manager: Arc<ReConnectionManager>,
}

impl EdenBuckOut {
    pub fn new(
        fb: FacebookInit,
        buck_out_path: ProjectRelativePathBuf,
        mount_point: AbsPathBuf,
        re_client_manager: Arc<ReConnectionManager>,
    ) -> anyhow::Result<Self> {
        setup(&mount_point)?;

        // Default to the number of CPUs. This value is very conservative
        // TODO (yipu): Benchmark and figure out optimal default concurrency
        static CONCURRENCY: EnvHelper<usize> = EnvHelper::new("BUCK2_EDEN_CONCURRENCY");
        let concurrency = CONCURRENCY.get()?.unwrap_or_else(num_cpus::get);
        let connection_manager =
            EdenConnectionManager::new(fb, &mount_point, Semaphore::new(concurrency))?
                .expect("EdenFS mount does not setup correctly");

        Ok(Self {
            buck_out_path,
            connection_manager,
            re_client_manager,
        })
    }

    pub async fn set_path_object_id(
        &self,
        path: &ProjectRelativePathBuf,
        value: &ArtifactValue,
    ) -> anyhow::Result<()> {
        // Eden's SetPathObjectId requires object_id and object_type, the object_id is a string of
        // the RE digest for a file or symlink target for a symlink or external symlink
        let (object_id, object_type) = get_object_id_and_type(value);
        let relpath_to_buck_out = path.strip_prefix(&self.buck_out_path).with_context(|| {
            format!(
                "Invalid artifact: path might not in the buck-out directory: {}",
                path
            )
        })?;

        let re_session_id = self
            .re_client_manager
            .get_re_connection()
            .get_client()
            .get_session_id()
            .await?;

        let params = SetPathObjectIdParams {
            mountPoint: self.connection_manager.get_mount_point(),
            path: relpath_to_buck_out.as_str().as_bytes().to_vec(),
            objectId: object_id.into_bytes(),
            r#type: object_type,
            mode: CheckoutMode::FORCE,
            requestInfo: Some(BTreeMap::from([(
                String::from("session-id"),
                re_session_id,
            )])),
            ..Default::default()
        };
        self.connection_manager
            .with_eden(move |eden| eden.setPathObjectId(&params))
            .await?;
        Ok(())
    }

    async fn remove_path_recursive(
        &self,
        project_fs: &ProjectFilesystem,
        path: &ProjectRelativePathBuf,
    ) -> anyhow::Result<()> {
        // Existence check would not trigger materialization since EdenFS will fast
        // return by not materialization the path. So the only cost is Eden will load
        // path's ancestors, which should be relatively inexpensive.
        if !project_fs.exists(path) {
            return Ok(());
        }

        let relpath_to_buck_out = path.strip_prefix(&self.buck_out_path).with_context(|| {
            format!(
                "Invalid artifact: path might not in the buck-out directory: {}",
                path
            )
        })?;

        dropcancel_critical_section(async move {
            let params = RemoveRecursivelyParams {
                mountPoint: self.connection_manager.get_mount_point(),
                path: relpath_to_buck_out.as_str().as_bytes().to_vec(),
                ..Default::default()
            };

            self.connection_manager
                .with_eden(move |eden| eden.removeRecursively(&params))
                .await?;
            Ok(())
        })
        .await
    }

    pub async fn remove_paths_recursive(
        &self,
        project_fs: &ProjectFilesystem,
        paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<()> {
        let futs = paths.iter().map(|path| async move {
            self.remove_path_recursive(project_fs, path)
                .await
                .with_context(|| format!("[eden] Error cleaning up path {}", path))
        });

        futures::future::try_join_all(futs).await?;

        Ok(())
    }

    pub async fn ensure_materialized(
        &self,
        paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<()> {
        let file_paths = paths
            .iter()
            .filter_map(|path| path.strip_prefix(&self.buck_out_path).ok())
            .map(|relpath| relpath.as_str().as_bytes().to_vec())
            .collect::<Vec<_>>();
        static ENSURE_MATERIALIZED_IN_BACKGROUND: EnvHelper<bool> =
            EnvHelper::new("BUCK2_EDEN_ENSURE_MATERIALIZED_IN_BACKGROUND");
        let background = ENSURE_MATERIALIZED_IN_BACKGROUND.get()?.unwrap_or(true);
        let params = EnsureMaterializedParams {
            mountPoint: self.connection_manager.get_mount_point(),
            paths: file_paths,
            background,
            followSymlink: true, // Also materialize symlink targets
            ..Default::default()
        };

        self.connection_manager
            .with_eden(move |eden| eden.ensureMaterialized(&params))
            .await?;
        Ok(())
    }
}

#[cfg(all(test, not(windows)))]
mod tests {
    use std::path::PathBuf;
    use std::sync::Arc;

    use buck2_common::external_symlink::ExternalSymlink;
    use buck2_common::file_ops::FileDigest;
    use buck2_common::file_ops::FileMetadata;
    use buck2_common::file_ops::TrackedFileDigest;
    use buck2_core::fs::paths::ForwardRelativePathBuf;
    use buck2_core::fs::paths::RelativePathBuf;
    use gazebo::prelude::*;

    use super::*;
    use crate::actions::directory::Symlink;

    #[test]
    fn test_symlink_object_id() -> anyhow::Result<()> {
        let symlink = Symlink::new(RelativePathBuf::from("path/include"));
        assert_eq!(
            "re-symlink:path/include",
            get_symlink_object_id(symlink.target().as_str())
        );
        Ok(())
    }

    #[test]
    fn test_external_symlink_object_id() -> anyhow::Result<()> {
        let symlink = ExternalSymlink::new(
            PathBuf::from("/mnt/gvfs"),
            Some(ForwardRelativePathBuf::unchecked_new("include".to_owned())),
        );
        assert_eq!(
            "re-symlink:/mnt/gvfs/include",
            get_symlink_object_id(Arc::new(symlink).to_path_buf().to_str().unwrap())
        );
        Ok(())
    }

    #[test]
    fn test_get_object_id_and_type_blob() -> anyhow::Result<()> {
        let digest = TrackedFileDigest::new(FileDigest::from_bytes("content".as_bytes()));
        let metadata_executable = FileMetadata {
            digest: digest.dupe(),
            is_executable: true,
        };

        let value = ArtifactValue::from(DirectoryEntry::Leaf(ActionDirectoryMember::File(
            metadata_executable,
        )));
        let (object_id, object_type) = get_object_id_and_type(&value);
        assert_eq!(digest.to_string(), object_id);
        assert_eq!(ObjectType::EXECUTABLE_FILE, object_type);

        let metadata_regular = FileMetadata {
            digest: digest.dupe(),
            is_executable: false,
        };

        let value = ArtifactValue::from(DirectoryEntry::Leaf(ActionDirectoryMember::File(
            metadata_regular,
        )));
        let (object_id, object_type) = get_object_id_and_type(&value);
        assert_eq!(digest.to_string(), object_id);
        assert_eq!(ObjectType::REGULAR_FILE, object_type);
        Ok(())
    }

    #[test]
    fn test_get_object_id_and_type_symlink() -> anyhow::Result<()> {
        let symlink = Symlink::new(RelativePathBuf::from("path/include"));
        let value = ArtifactValue::from(DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(
            Arc::new(symlink),
        )));
        let (object_id, object_type) = get_object_id_and_type(&value);
        assert_eq!("re-symlink:path/include", object_id);
        assert_eq!(ObjectType::SYMLINK, object_type);
        Ok(())
    }

    #[test]
    fn test_get_object_id_and_type_external_symlink() -> anyhow::Result<()> {
        let symlink = Arc::new(ExternalSymlink::new(
            PathBuf::from("/mnt/gvfs"),
            Some(ForwardRelativePathBuf::unchecked_new("include".to_owned())),
        ));

        let value = ArtifactValue::from(DirectoryEntry::Leaf(
            ActionDirectoryMember::ExternalSymlink(symlink),
        ));
        let (object_id, object_type) = get_object_id_and_type(&value);
        assert_eq!("re-symlink:/mnt/gvfs/include", object_id);
        assert_eq!(ObjectType::SYMLINK, object_type);
        Ok(())
    }
}
