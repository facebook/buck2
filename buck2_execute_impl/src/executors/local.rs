/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_common::liveliness_manager::LivelinessManager;
use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::paths::FileNameBuf;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::process::background_command;
use buck2_execute::artifact::fs::ArtifactFs;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::directory::extract_artifact_value;
use buck2_execute::directory::insert_entry;
use buck2_execute::directory::new_symlink;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::clean_output_paths::CleanOutputPaths;
use buck2_execute::execute::environment_inheritance::EnvironmentInheritance;
use buck2_execute::execute::inputs_directory::inputs_directory;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::manager::CommandExecutionManagerExt;
use buck2_execute::execute::output::CommandStdStreams;
use buck2_execute::execute::prepared::PreparedCommand;
use buck2_execute::execute::prepared::PreparedCommandExecutor;
use buck2_execute::execute::request::CommandExecutionInput;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::CommandExecutionOutputRef;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::execute::result::CommandExecutionTimingData;
use buck2_execute::execute::target::CommandExecutionTarget;
use buck2_execute::knobs::ExecutorGlobalKnobs;
use buck2_execute::materialize::materializer::Materializer;
use buck2_forkserver::client::ForkserverClient;
use buck2_forkserver::run::gather_output;
use buck2_forkserver::run::timeout_into_cancellation;
use buck2_forkserver::run::GatherOutputStatus;
use derive_more::From;
use faccess::PathExt;
use futures::future;
use futures::future::select;
use futures::future::FutureExt;
use gazebo::prelude::*;
use host_sharing::HostSharingBroker;
use indexmap::IndexMap;
use more_futures::spawn::dropcancel_critical_section;
use remote_execution as RE;
use thiserror::Error;
use tracing::info;

#[derive(Debug, Error)]
enum LocalExecutionError {
    #[error("Args list was empty")]
    NoArgs,
}

#[derive(Clone)]
pub struct LocalExecutor {
    artifact_fs: ArtifactFs,
    materializer: Arc<dyn Materializer>,
    blocking_executor: Arc<dyn BlockingExecutor>,
    host_sharing_broker: Arc<HostSharingBroker>,
    root: AbsPathBuf,
    #[cfg_attr(not(unix), allow(unused))]
    forkserver: Option<ForkserverClient>,
    #[allow(unused)]
    knobs: ExecutorGlobalKnobs,
}

impl LocalExecutor {
    pub fn new(
        artifact_fs: ArtifactFs,
        materializer: Arc<dyn Materializer>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        host_sharing_broker: Arc<HostSharingBroker>,
        root: AbsPathBuf,
        forkserver: Option<ForkserverClient>,
        knobs: ExecutorGlobalKnobs,
    ) -> Self {
        Self {
            artifact_fs,
            materializer,
            blocking_executor,
            host_sharing_broker,
            root,
            forkserver,
            knobs,
        }
    }

    // Compiler gets confused (on the not(unix) branch only, weirdly) if you use an async fn.
    #[allow(clippy::manual_async_fn)]
    fn exec<'a>(
        &'a self,
        exe: &'a str,
        args: impl IntoIterator<Item = impl AsRef<OsStr>> + 'a,
        env: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)> + 'a,
        working_directory: Option<&'a ProjectRelativePath>,
        timeout: Option<Duration>,
        env_inheritance: Option<&'a EnvironmentInheritance>,
        liveliness_manager: Arc<dyn LivelinessManager>,
    ) -> impl futures::future::Future<Output = anyhow::Result<(GatherOutputStatus, Vec<u8>, Vec<u8>)>> + 'a
    {
        async move {
            let working_directory = match working_directory {
                Some(d) => Cow::Owned(self.root.join(d)),
                None => Cow::Borrowed(&self.root),
            };

            let working_directory: &Path = working_directory.as_ref();

            match &self.forkserver {
                Some(forkserver) => {
                    #[cfg(unix)]
                    {
                        unix::exec_via_forkserver(
                            forkserver,
                            exe,
                            args,
                            env,
                            working_directory,
                            timeout,
                            env_inheritance,
                            liveliness_manager,
                        )
                        .await
                    }

                    #[cfg(not(unix))]
                    {
                        let _unused = forkserver;
                        Err(anyhow::anyhow!("Forkserver is not supported off-UNIX"))
                    }
                }

                None => {
                    let mut cmd = background_command(exe);
                    cmd.current_dir(working_directory);
                    cmd.args(args);
                    apply_local_execution_environment(
                        &mut cmd,
                        working_directory,
                        env,
                        env_inheritance,
                    );
                    let timeout = timeout_into_cancellation(timeout);
                    let alive = liveliness_manager
                        .while_alive()
                        .map(|()| anyhow::Ok(GatherOutputStatus::Cancelled));
                    let cancellation =
                        select(timeout.boxed(), alive.boxed()).map(|r| r.factor_first().0);
                    gather_output(cmd, cancellation).await
                }
                .with_context(|| format!("Failed to gather output from command: {}", exe)),
            }
        }
    }

    async fn exec_request(
        &self,
        action_digest: &ActionDigest,
        action: CommandExecutionTarget<'_>,
        request: &CommandExecutionRequest,
        mut manager: CommandExecutionManager,
    ) -> CommandExecutionResult {
        let args = request.args();
        if args.is_empty() {
            return manager.error("no_args".into(), LocalExecutionError::NoArgs.into());
        }

        match manager
            .stage_async(
                buck2_data::LocalStage {
                    stage: Some(buck2_data::LocalMaterializeInputs {}.into()),
                },
                async {
                    let (r1, r2) = future::join(
                        materialize_inputs(&self.artifact_fs, &self.materializer, request),
                        async {
                            // When user requests to not perform a cleanup for a specific action
                            // output from previous run of that action could actually be used as the
                            // input during current run (e.g. extra output which is an incremental state describing the actual output).
                            if !request.outputs_cleanup {
                                materialize_build_outputs_from_previous_run(
                                    &self.artifact_fs,
                                    &self.materializer,
                                    request,
                                )
                                .await
                            } else {
                                Ok(())
                            }
                        },
                    )
                    .await;
                    r1.and(r2)
                },
            )
            .await
        {
            Ok(_) => {}
            Err(e) => return manager.error("materialize_inputs_failed".into(), e),
        };

        let mut manager = manager.claim().await;

        let scratch_dir = self
            .artifact_fs
            .buck_out_path_resolver()
            .resolve_scratch(&action.scratch_dir());
        // For the $TMPDIR - important it is absolute
        let scratch_dir_abs = self.artifact_fs.fs().resolve(&scratch_dir);

        if let Err(e) = manager
            .stage_async(
                buck2_data::LocalStage {
                    stage: Some(buck2_data::LocalPrepareOutputDirs {}.into()),
                },
                async move {
                    // TODO(cjhopman): This should be getting the action exec context so it get use io_blocking_section
                    if request.custom_tmpdir {
                        let project_fs = self.artifact_fs.fs();
                        project_fs.remove_path_recursive(&scratch_dir)?;
                        fs_util::create_dir_all(&*project_fs.resolve(&scratch_dir))?;
                    }

                    create_output_dirs(
                        &self.artifact_fs,
                        request,
                        self.materializer.dupe(),
                        self.blocking_executor.dupe(),
                    )
                    .await
                    .context("Error creating output directories")?;

                    Ok(())
                },
            )
            .await
        {
            return manager.error("prepare_output_dirs_failed".into(), e);
        };

        info!(
            "Local execution command line:\n```\n$ {}\n```",
            args.join(" "),
        );

        let tmpdir = if request.custom_tmpdir {
            Some(("TMPDIR", scratch_dir_abs.as_os_str()))
        } else {
            None
        };

        let iter_env = || {
            tmpdir
                .into_iter()
                .map(|(k, v)| (k, StrOrOsStr::from(v)))
                .chain(
                    request
                        .env()
                        .iter()
                        .map(|(k, v)| (k.as_str(), StrOrOsStr::from(v.as_str()))),
                )
        };

        let liveliness_manager = manager.liveliness_manager.dupe();

        let (timing, res) = manager
            .stage_async(
                {
                    let env = iter_env()
                        .map(|(k, v)| buck2_data::local_command::EnvironmentEntry {
                            key: k.to_owned(),
                            value: v.into_string_lossy(),
                        })
                        .collect();
                    let stage = buck2_data::LocalExecute {
                        command: Some(buck2_data::LocalCommand {
                            action_digest: action_digest.to_string(),
                            argv: args.to_vec(),
                            env,
                        }),
                    };
                    buck2_data::LocalStage {
                        stage: Some(stage.into()),
                    }
                },
                async move {
                    let execution_start = Instant::now();
                    let start_time = SystemTime::now();

                    let env = iter_env().map(|(k, v)| (k, v.into_os_str()));
                    let r = self
                        .exec(
                            &args[0],
                            &args[1..],
                            env,
                            request.working_directory(),
                            request.timeout(),
                            request.local_environment_inheritance(),
                            liveliness_manager,
                        )
                        .await;

                    let execution_time = execution_start.elapsed();

                    let timing = CommandExecutionTimingData {
                        wall_time: execution_time,
                        re_queue_time: None,
                        execution_time,
                        start_time,
                    };

                    (timing, r)
                },
            )
            .await;

        let execution_kind = CommandExecutionKind::Local {
            digest: action_digest.dupe(),
            command: args.to_vec(),
            env: request.env().clone(),
        };

        let (status, stdout, stderr) = match res {
            Ok(res) => res,
            Err(e) => return manager.error("exec_failed".into(), e), // TODO (torozco): Can this take CommandExecutionKind? Should this be a failure?
        };

        let std_streams = CommandStdStreams::Local { stdout, stderr };

        match status {
            GatherOutputStatus::Finished(status) => {
                let outputs = match self.calculate_and_declare_output_values(request).await {
                    Ok(output_values) => output_values,
                    Err(e) => return manager.error("calculate_output_values_failed".into(), e),
                };

                match status.code() {
                    Some(0) => manager.success(execution_kind, outputs, std_streams, timing),
                    v => manager.failure(execution_kind, outputs, std_streams, v),
                }
            }
            GatherOutputStatus::TimedOut(duration) => {
                manager.timeout(execution_kind, duration, std_streams, timing)
            }
            GatherOutputStatus::Cancelled => manager.cancel_claim(),
        }
    }

    async fn calculate_and_declare_output_values(
        &self,
        request: &CommandExecutionRequest,
    ) -> anyhow::Result<IndexMap<CommandExecutionOutput, ArtifactValue>> {
        let mut builder = inputs_directory(request.inputs(), &self.artifact_fs)?;

        // Read outputs from disk and add them to the builder
        let mut entries = Vec::new();
        for output in request.outputs() {
            let path = output.resolve(&self.artifact_fs).into_path();
            let abspath = self.root.join(&path);
            let entry = self
                .build_entry_from_disk(abspath.to_path_buf())
                .with_context(|| format!("collecting output {:?}", path))?;
            if let Some(entry) = entry {
                insert_entry(&mut builder, path.as_ref(), entry)?;
                entries.push((output.cloned(), path));
            }
        }

        let mut to_declare = vec![];
        let mut mapped_outputs = IndexMap::with_capacity(entries.len());

        for (output, path) in entries {
            let value = extract_artifact_value(&builder, path.as_ref())?;
            if let Some(value) = value {
                match output {
                    CommandExecutionOutput::BuildArtifact(..) => {
                        to_declare.push((path, value.dupe()));
                    }
                    CommandExecutionOutput::TestPath { .. } => {
                        // Don't declare those as we don't currently have any form of GC so this
                        // would take up space for nothing, and most importantly, we will never
                        // need them to be in materializer state for e.g. matching as nothign
                        // should depend on them.
                    }
                }

                mapped_outputs.insert(output, value);
            }
        }

        if self.knobs.declare_in_local_executor {
            self.materializer.declare_existing(to_declare).await?;
        }

        Ok(mapped_outputs)
    }

    fn build_entry_from_disk(
        &self,
        mut path: PathBuf,
    ) -> anyhow::Result<Option<ActionDirectoryEntry<ActionDirectoryBuilder>>> {
        fn build_dir_from_disk(disk_path: &mut PathBuf) -> anyhow::Result<ActionDirectoryBuilder> {
            let mut builder = ActionDirectoryBuilder::empty();

            for file in fs_util::read_dir(&disk_path)? {
                let file = file?;
                let filetype = file.file_type()?;
                let filename = file.file_name();
                disk_path.push(filename.as_os_str());

                let filename = filename
                    .to_str()
                    .context("Filename is not UTF-8")
                    .and_then(|f| FileNameBuf::try_from(f.to_owned()))
                    .with_context(|| format!("Invalid filename: {}", disk_path.display()))?;

                if filetype.is_dir() {
                    let dir = build_dir_from_disk(disk_path)?;
                    builder.insert(filename, DirectoryEntry::Dir(dir))?;
                } else if filetype.is_symlink() {
                    builder.insert(
                        filename,
                        DirectoryEntry::Leaf(new_symlink(fs_util::read_link(&disk_path)?)?),
                    )?;
                } else if filetype.is_file() {
                    let metadata = FileMetadata {
                        digest: TrackedFileDigest::new(FileDigest::from_file(&disk_path)?),
                        is_executable: file.path().executable(),
                    };
                    builder.insert(
                        filename,
                        DirectoryEntry::Leaf(ActionDirectoryMember::File(metadata)),
                    )?;
                }
                disk_path.pop();
            }

            Ok(builder)
        }

        // Get file metadata. If the file is missing, ignore it.
        let m = match std::fs::symlink_metadata(&path) {
            Ok(m) => m,
            Err(ref err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(err) => return Err(err.into()),
        };

        let value = if m.file_type().is_symlink() {
            DirectoryEntry::Leaf(new_symlink(fs_util::read_link(&path)?)?)
        } else if m.is_file() {
            DirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata {
                digest: TrackedFileDigest::new(FileDigest::from_file(&path)?),
                is_executable: path.executable(),
            }))
        } else if m.is_dir() {
            DirectoryEntry::Dir(build_dir_from_disk(&mut path)?)
        } else {
            unimplemented!("Path {:?} is of an unknown file type.", path)
        };
        Ok(Some(value))
    }
}

#[async_trait]
impl PreparedCommandExecutor for LocalExecutor {
    async fn exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        mut manager: CommandExecutionManager,
    ) -> CommandExecutionResult {
        let PreparedCommand {
            request,
            target,
            action_paths: _action_paths,
            prepared_action,
        } = command;
        let _permit = manager
            .stage_async(
                buck2_data::LocalStage {
                    stage: Some(buck2_data::LocalQueued {}.into()),
                },
                self.host_sharing_broker
                    .acquire(request.host_sharing_requirements()),
            )
            .await;

        // If we start running something, we don't want this task to get dropped, because if we do
        // we might interfere with e.g. clean up.
        dropcancel_critical_section(Self::exec_request(
            self,
            &prepared_action.action,
            *target,
            request,
            manager,
        ))
        .await
    }

    fn re_platform(&self) -> Option<&RE::Platform> {
        None
    }

    fn re_use_case(&self) -> RemoteExecutorUseCase {
        RemoteExecutorUseCase::buck2_default()
    }
}

/// Either a str or a OsStr, so that we can turn it back into a String without having to check for
/// valid utf-8, while using the same struct.
#[derive(Copy, Clone, Dupe, From)]
enum StrOrOsStr<'a> {
    Str(&'a str),
    OsStr(&'a OsStr),
}

impl<'a> StrOrOsStr<'a> {
    fn into_string_lossy(self) -> String {
        match self {
            Self::Str(s) => s.to_owned(),
            Self::OsStr(s) => s.to_string_lossy().into_owned(),
        }
    }

    fn into_os_str(self) -> &'a OsStr {
        match self {
            Self::Str(s) => OsStr::new(s),
            Self::OsStr(s) => s,
        }
    }
}

/// Materialize all inputs artifact for CommandExecutionRequest so the command can be executed locally.
pub async fn materialize_inputs(
    artifact_fs: &ArtifactFs,
    materializer: &Arc<dyn Materializer>,
    request: &CommandExecutionRequest,
) -> anyhow::Result<()> {
    let mut paths = vec![];

    for input in request.inputs() {
        match input {
            CommandExecutionInput::Artifact(group) => {
                for (artifact, _) in group.iter() {
                    if !artifact.is_source() {
                        paths.push(artifact_fs.resolve(artifact.get_path())?);
                    }
                }
            }
            CommandExecutionInput::ActionMetadata(metadata) => {
                let path = artifact_fs
                    .buck_out_path_resolver()
                    .resolve_gen(&metadata.path);
                CleanOutputPaths::clean(std::iter::once(path.as_ref()), artifact_fs.fs())?;
                artifact_fs.fs().write_file(&path, &metadata.data, false)?;
            }
        }
    }

    materializer.ensure_materialized(paths).await
}

/// Materialize build outputs from the previous run of the same command.
/// Useful when executing incremental actions first remotely and then locally.
/// In that case output from remote execution which is incremental state should be materialized prior local execution.
/// Such incremental state in fact serves as the input while being output as well.
pub async fn materialize_build_outputs_from_previous_run(
    artifact_fs: &ArtifactFs,
    materializer: &Arc<dyn Materializer>,
    request: &CommandExecutionRequest,
) -> anyhow::Result<()> {
    let mut paths = vec![];

    for output in request.outputs() {
        match output {
            CommandExecutionOutputRef::BuildArtifact(artifact) => {
                paths.push(artifact_fs.resolve_build(artifact));
            }
            CommandExecutionOutputRef::TestPath { path: _, create: _ } => {}
        }
    }

    materializer.ensure_materialized(paths).await
}

/// Create any output dirs requested by the command. Note that this makes no effort to delete
/// the output paths first. Eventually it should, but right now this happens earlier. This
/// would be a separate refactor.
pub async fn create_output_dirs(
    artifact_fs: &ArtifactFs,
    request: &CommandExecutionRequest,
    materializer: Arc<dyn Materializer>,
    blocking_executor: Arc<dyn BlockingExecutor>,
) -> anyhow::Result<()> {
    let outputs: Vec<_> = request
        .outputs()
        .map(|output| output.resolve(artifact_fs))
        .collect();

    if request.outputs_cleanup {
        // Invalidate all the output paths this action might provide. Note that this is a bit
        // approximative: we might have previous instances of this action that declared
        // different outputs with a different materialization method that will become invalid
        // now. However, nothing should reference those stale outputs, so while this does not
        // do a good job of cleaning up garbage, it prevents using invalid artifacts.
        let outputs = outputs.map(|output| output.path.to_owned());
        materializer.invalidate_many(outputs.clone()).await?;

        // TODO(scottcao): Move this deletion logic into materializer itself.
        // Use Eden's clean up API if possible, it is significantly faster on Eden compared with
        // the native method as the API does not load and materialize files or folders
        if let Some(eden_buck_out) = materializer.eden_buck_out() {
            eden_buck_out
                .remove_paths_recursive(artifact_fs.fs(), outputs)
                .await?;
        } else {
            blocking_executor
                .execute_io(box CleanOutputPaths { paths: outputs })
                .await
                .context("Failed to cleanup output directory")?;
        }
    }

    let project_fs = artifact_fs.fs();
    for output in outputs {
        if let Some(path) = output.path_to_create() {
            fs_util::create_dir_all(project_fs.resolve(path))?;
        }
    }

    Ok(())
}

pub fn apply_local_execution_environment(
    builder: &mut impl EnvironmentBuilder,
    working_directory: &Path,
    env: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>,
    env_inheritance: Option<&EnvironmentInheritance>,
) {
    if let Some(env_inheritance) = env_inheritance {
        if env_inheritance.clear() {
            builder.clear();
        }

        for key in env_inheritance.exclusions() {
            builder.remove(key);
        }

        for (key, val) in env_inheritance.values() {
            builder.set(key, val);
        }
    }
    for (key, val) in env {
        builder.set(key, val);
    }
    builder.set("PWD", working_directory);
}

pub trait EnvironmentBuilder {
    fn clear(&mut self);

    fn set<K, V>(&mut self, key: K, val: V)
    where
        K: AsRef<OsStr>,
        V: AsRef<OsStr>;

    fn remove<K>(&mut self, key: K)
    where
        K: AsRef<OsStr>;
}

impl EnvironmentBuilder for Command {
    fn clear(&mut self) {
        Command::env_clear(self);
    }

    fn set<K, V>(&mut self, key: K, val: V)
    where
        K: AsRef<OsStr>,
        V: AsRef<OsStr>,
    {
        Command::env(self, key, val);
    }

    fn remove<K>(&mut self, key: K)
    where
        K: AsRef<OsStr>,
    {
        Command::env_remove(self, key);
    }
}

#[cfg(unix)]
mod unix {
    use std::os::unix::ffi::OsStrExt;

    use buck2_execute::execute::environment_inheritance::EnvironmentInheritance;

    use super::*;

    pub async fn exec_via_forkserver(
        forkserver: &ForkserverClient,
        exe: impl AsRef<OsStr>,
        args: impl IntoIterator<Item = impl AsRef<OsStr>>,
        env: impl IntoIterator<Item = (impl AsRef<OsStr>, impl AsRef<OsStr>)>,
        working_directory: &Path,
        comand_timeout: Option<Duration>,
        env_inheritance: Option<&EnvironmentInheritance>,
        liveliness_manager: Arc<dyn LivelinessManager>,
    ) -> anyhow::Result<(GatherOutputStatus, Vec<u8>, Vec<u8>)> {
        let exe = exe.as_ref();

        let mut req = buck2_forkserver_proto::CommandRequest {
            exe: exe.as_bytes().to_vec(),
            argv: args
                .into_iter()
                .map(|s| s.as_ref().as_bytes().to_vec())
                .collect(),
            env: vec![],
            env_clear: false,
            env_remove: vec![],
            cwd: Some(buck2_forkserver_proto::WorkingDirectory {
                path: working_directory.as_os_str().as_bytes().to_vec(),
            }),
            timeout: comand_timeout.map(|d| d.into()),
        };
        apply_local_execution_environment(&mut req, working_directory, env, env_inheritance);
        forkserver
            .execute(req, liveliness_manager.while_alive_owned())
            .await
    }

    impl EnvironmentBuilder for buck2_forkserver_proto::CommandRequest {
        fn clear(&mut self) {
            self.env_clear = true;
        }

        fn set<K, V>(&mut self, key: K, val: V)
        where
            K: AsRef<OsStr>,
            V: AsRef<OsStr>,
        {
            self.env.push(buck2_forkserver_proto::EnvVar {
                key: key.as_ref().as_bytes().to_vec(),
                value: val.as_ref().as_bytes().to_vec(),
            })
        }

        fn remove<K>(&mut self, key: K)
        where
            K: AsRef<OsStr>,
        {
            self.env_remove.push(key.as_ref().as_bytes().to_vec())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::str;
    use std::sync::Arc;
    use std::time::Instant;

    use buck2_common::liveliness_manager::NoopLivelinessManager;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::testing::CellResolverExt;
    use buck2_core::cells::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_execute::artifact::fs::ArtifactFs;
    use buck2_execute::execute::blocking::testing::DummyBlockingExecutor;
    use buck2_execute::materialize::nodisk::NoDiskMaterializer;
    use buck2_execute::path::buck_out_path::BuckOutPathResolver;
    use buck2_execute::path::buck_out_path::BuckPathResolver;
    use host_sharing::HostSharingStrategy;

    use super::*;

    #[tokio::test]
    async fn test_gather_output() -> anyhow::Result<()> {
        let mut cmd = if cfg!(windows) {
            background_command("powershell")
        } else {
            background_command("sh")
        };
        cmd.args(&["-c", "echo hello"]);

        let (status, stdout, stderr) = gather_output(cmd, futures::future::pending()).await?;
        assert!(matches!(status, GatherOutputStatus::Finished(s) if s.code() == Some(0)));
        assert_eq!(str::from_utf8(&stdout)?.trim(), "hello");
        assert_eq!(stderr, b"");

        Ok(())
    }

    #[tokio::test]
    async fn test_gather_does_not_wait_for_children() -> anyhow::Result<()> {
        // If we wait for sleep, this will time out.
        let mut cmd = if cfg!(windows) {
            background_command("powershell")
        } else {
            background_command("sh")
        };
        if cfg!(windows) {
            cmd.args(&[
                "-c",
                "Start-Job -ScriptBlock {sleep 10} | Out-Null; echo hello",
            ]);
        } else {
            cmd.args(&["-c", "(sleep 10 &) && echo hello"]);
        }

        let timeout = if cfg!(windows) { 5 } else { 1 };
        let (status, stdout, stderr) = gather_output(
            cmd,
            timeout_into_cancellation(Some(Duration::from_secs(timeout))),
        )
        .await?;
        assert!(matches!(status, GatherOutputStatus::Finished(s) if s.code() == Some(0)));
        assert_eq!(str::from_utf8(&stdout)?.trim(), "hello");
        assert_eq!(stderr, b"");

        Ok(())
    }

    #[tokio::test]
    async fn test_gather_output_timeout() -> anyhow::Result<()> {
        let now = Instant::now();

        let mut cmd = if cfg!(windows) {
            background_command("powershell")
        } else {
            background_command("sh")
        };
        cmd.args(&["-c", "echo hello; sleep 10; echo bye"]);

        let timeout = if cfg!(windows) { 5 } else { 1 };
        let (status, stdout, stderr) = gather_output(
            cmd,
            timeout_into_cancellation(Some(Duration::from_secs(timeout))),
        )
        .await?;
        assert!(matches!(status, GatherOutputStatus::TimedOut(..)));
        assert_eq!(str::from_utf8(&stdout)?.trim(), "hello");
        assert_eq!(stderr, b"");

        assert!(now.elapsed() < Duration::from_secs(9)); // Lots of leeway here.

        Ok(())
    }

    fn artifact_fs(project_fs: ProjectRoot) -> ArtifactFs {
        ArtifactFs::new(
            BuckPathResolver::new(CellResolver::of_names_and_paths(&[(
                CellName::unchecked_new("cell".into()),
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell_path".into())),
            )])),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck_out/v2".into())),
            project_fs,
        )
    }

    fn test_executor() -> anyhow::Result<(LocalExecutor, AbsPathBuf, impl Drop)> {
        let dir = tempfile::tempdir()?;
        let root = AbsPathBuf::try_from(dir.path().canonicalize()?)?;
        let project_fs = ProjectRoot::new(root.clone());
        let artifact_fs = artifact_fs(project_fs.dupe());

        let executor = LocalExecutor::new(
            artifact_fs,
            Arc::new(NoDiskMaterializer),
            Arc::new(DummyBlockingExecutor { fs: project_fs }),
            Arc::new(HostSharingBroker::new(
                HostSharingStrategy::SmallerTasksFirst,
                1,
            )),
            root.clone(),
            None,
            ExecutorGlobalKnobs::default(),
        );

        Ok((executor, root, dir))
    }

    #[tokio::test]
    async fn test_exec_cmd_environment() -> anyhow::Result<()> {
        let (executor, root, _tmpdir) = test_executor()?;

        let interpreter = if cfg!(windows) { "powershell" } else { "sh" };
        let (status, stdout, _) = executor
            .exec(
                interpreter,
                &["-c", "echo $PWD; pwd"],
                &HashMap::<String, String>::default(),
                None,
                None,
                None,
                NoopLivelinessManager::create(),
            )
            .await?;
        assert!(matches!(status, GatherOutputStatus::Finished(s) if s.code() == Some(0)));

        let stdout = std::str::from_utf8(&stdout).context("Invalid stdout")?;

        if cfg!(windows) {
            let lines: Vec<&str> = stdout.split("\r\n").collect();
            let expected_path = format!("Microsoft.PowerShell.Core\\FileSystem::{}", root);

            assert_eq!(lines[3], expected_path);
            assert_eq!(lines[4], expected_path);
        } else {
            assert_eq!(stdout, format!("{}\n{}\n", root, root));
        }

        Ok(())
    }

    #[cfg(unix)] // TODO: something similar on Windows: T123279320
    #[tokio::test]
    async fn test_exec_cmd_environment_filtering() -> anyhow::Result<()> {
        use buck2_execute::execute::environment_inheritance::EnvironmentInheritance;

        let (executor, _root, _tmpdir) = test_executor()?;

        let (status, stdout, _) = executor
            .exec(
                "sh",
                &["-c", "echo $USER"],
                &HashMap::<String, String>::default(),
                None,
                None,
                Some(&EnvironmentInheritance::empty()),
                NoopLivelinessManager::create(),
            )
            .await?;
        assert!(matches!(status, GatherOutputStatus::Finished(s) if s.code() == Some(0)));
        assert_eq!(stdout, "\n".as_bytes());

        Ok(())
    }
}
