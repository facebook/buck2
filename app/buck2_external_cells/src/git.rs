/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::hash_map;
use std::collections::HashMap;
use std::process::Command;
use std::process::ExitStatus;
use std::process::Stdio;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;

use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::dice::file_ops::delegate::FileOpsDelegate;
use buck2_common::file_ops::FileDigestConfig;
use buck2_common::file_ops::RawDirEntry;
use buck2_common::file_ops::RawPathMetadata;
use buck2_common::io::fs::FsIoProvider;
use buck2_common::io::IoProvider;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::external::ExternalCellOrigin;
use buck2_core::cells::external::GitCellSetup;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::fs::buck_out_path::BuckOutPathResolver;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_directory::directory::directory::Directory;
use buck2_error::internal_error;
use buck2_error::BuckErrorContext;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::directory::INTERNER;
use buck2_execute::entry::build_entry_from_disk;
use buck2_execute::execute::blocking::HasBlockingExecutor;
use buck2_execute::execute::blocking::IoRequest;
use buck2_execute::execute::clean_output_paths::CleanOutputPaths;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_execute::materialize::materializer::Materializer;
use buck2_util::process::background_command;
use cmp_any::PartialEqAny;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use tokio::sync::Semaphore;

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
enum GitError {
    #[error("Error fetching external cell with git, exit code: {exit_code:?}, stderr:\n{stderr}")]
    Unsuccessful {
        exit_code: ExitStatus,
        stderr: String,
    },
    #[error("Expected git to create a directory at the checkout location")]
    NoDirectory,
}

struct GitFetchIoRequest {
    setup: GitCellSetup,
    path: ProjectRelativePathBuf,
}

impl IoRequest for GitFetchIoRequest {
    fn execute(
        self: Box<Self>,
        project_fs: &buck2_core::fs::project::ProjectRoot,
    ) -> buck2_error::Result<()> {
        let path = project_fs.resolve(&self.path);
        fs_util::create_dir_all(path.clone())?;

        // FIXME(JakobDegen): Ideally we'd use libgit2 directly here instead of shelling out, but
        // unfortunately the third party situation for that library in fbsource isn't great, so
        // let's do this for now
        fn run_git(cwd: &AbsNormPath, f: impl FnOnce(&mut Command)) -> buck2_error::Result<()> {
            let mut cmd = background_command("git");
            f(&mut cmd);
            let output = cmd
                .current_dir(cwd)
                .stderr(Stdio::piped())
                .stdout(Stdio::null())
                .output()
                .buck_error_context("Could not run git to fetch external cell")?;

            if !output.status.success() {
                return Err(GitError::Unsuccessful {
                    exit_code: output.status,
                    stderr: String::from_utf8_lossy(&output.stderr).to_string(),
                }
                .into());
            }

            Ok(())
        }

        run_git(&path, |c| {
            c.arg("init");
        })?;

        run_git(&path, |c| {
            c.arg("remote")
                .arg("add")
                .arg("origin")
                .arg(self.setup.git_origin.as_ref());
        })?;

        run_git(&path, |c| {
            c.arg("fetch").arg("origin").arg(self.setup.commit.as_ref());
        })?;

        run_git(&path, |c| {
            c.arg("reset").arg("--hard").arg("FETCH_HEAD");
        })?;

        Ok(())
    }
}

async fn download_impl(
    ctx: &mut DiceComputations<'_>,
    setup: &GitCellSetup,
    path: &ProjectRelativePath,
    materializer: &dyn Materializer,
    cancellations: &CancellationContext,
) -> buck2_error::Result<()> {
    let io = ctx.get_blocking_executor();
    io.execute_io(
        Box::new(CleanOutputPaths {
            paths: vec![path.to_owned()],
        }),
        cancellations,
    )
    .await?;

    io.execute_io(
        Box::new(GitFetchIoRequest {
            setup: setup.dupe(),
            path: path.to_owned(),
        }),
        cancellations,
    )
    .await?;

    // Unfortunately, there's no way to ask git not to create this, but it's important that we
    // delete it so that we don't use it or waste cycles hashing it.
    io.execute_io(
        Box::new(CleanOutputPaths {
            paths: vec![path.join(ForwardRelativePath::new(".git").unwrap())],
        }),
        cancellations,
    )
    .await?;

    // Read and hash the contents. We have to do this because the materializer requires an artifact
    // value. This work is kind of duplicated with the reading in the fileops, but only the first
    // time the contents are downloaded. On subsequent invocations of the daemon, we won't rerun
    // this however, so that case will still avoid doing unnecessary work.
    let io_prov = ctx.global_data().get_io_provider();
    let proj_root = io_prov.project_root().root();
    let abs_path = proj_root.join(path);
    let digest_config = ctx.global_data().get_digest_config();
    let file_digest_config = FileDigestConfig::build(digest_config.cas_digest_config());
    let entry = build_entry_from_disk(abs_path, file_digest_config, &*io, proj_root)
        .await?
        .0
        .ok_or(GitError::NoDirectory)?;
    let entry = entry.map_dir(|d| {
        d.to_builder()
            .fingerprint(digest_config.as_directory_serializer())
            .shared(&*INTERNER)
    });

    materializer
        .declare_existing(vec![(path.to_owned(), ArtifactValue::new(entry, None))])
        .await?;

    Ok(())
}

async fn download_and_materialize(
    ctx: &mut DiceComputations<'_>,
    path: &ProjectRelativePath,
    setup: &GitCellSetup,
    cancellations: &CancellationContext,
) -> buck2_error::Result<()> {
    let materializer = ctx.per_transaction_data().get_materializer();

    if materializer.has_artifact_at(path.to_owned()).await? {
        return Ok(());
    }

    // A map of commit hashes to semaphores that are actually condvars which protect access to the
    // directory associated with that commit
    static DIRECTORY_LICENSES: OnceLock<Mutex<HashMap<Arc<str>, Arc<Semaphore>>>> = OnceLock::new();

    // We have to write this in a slightly funny way to convince the compiler that there's no
    // `map_guard` being held across an await point
    let semaphore;
    let semaphore_guard;
    'populate: {
        'wait: {
            let mut map_guard = DIRECTORY_LICENSES
                .get_or_init(Default::default)
                .lock()
                .unwrap();
            let entry = map_guard.entry(setup.commit.dupe());

            match entry {
                hash_map::Entry::Occupied(entry) => {
                    // There's another key simultaneously populating this directory. Just wait for
                    // it to finish and then return. We don't need to check the contents of the
                    // directory, since we assume that the commit hash uniquely identifies those.
                    semaphore = entry.get().dupe();
                    break 'wait;
                }
                hash_map::Entry::Vacant(entry) => {
                    // It's on us to populate this directory. Make a condvar so that we block other accesses
                    semaphore = Arc::new(Semaphore::new(1));
                    semaphore_guard = semaphore.try_acquire().unwrap(); // we know there's a permit available
                    entry.insert(semaphore.dupe());
                    break 'populate;
                }
            }
        }

        drop(semaphore.acquire().await.unwrap());
        return Ok(());
    }

    // Don't allow the actual download step to be cancelled. In principle it might be possible to
    // properly clean up after a cancellation within the execution of this key, but we'd also have
    // to deal with another key that might be waiting on this download to finish, which would be
    // pretty complicated to deal with.
    let res = cancellations
        .critical_section(|| download_impl(ctx, setup, path, &*materializer, cancellations))
        .await;

    // Give up our lock
    drop(semaphore_guard);
    DIRECTORY_LICENSES
        .get()
        .unwrap()
        .lock()
        .unwrap()
        .remove(&setup.commit)
        .unwrap();

    res
}

#[derive(allocative::Allocative)]
pub(crate) struct GitFileOpsDelegate {
    buck_out_resolver: BuckOutPathResolver,
    cell: CellName,
    setup: GitCellSetup,
    // The fs accesses in this code are sort of a mix between source file accesses and buck-out
    // accesses. Unconditionally using an `FsIoProvider` turns out to give all the right behavior
    io: FsIoProvider,
}

impl GitFileOpsDelegate {
    fn resolve(&self, path: &CellRelativePath) -> ProjectRelativePathBuf {
        self.buck_out_resolver
            .resolve_external_cell_source(path, ExternalCellOrigin::Git(self.setup.dupe()))
    }

    fn get_base_path(&self) -> ProjectRelativePathBuf {
        self.resolve(CellRelativePath::empty())
    }
}

#[async_trait::async_trait]
impl FileOpsDelegate for GitFileOpsDelegate {
    async fn read_file_if_exists(
        &self,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<String>> {
        let project_path = self.resolve(path);
        (&self.io as &dyn IoProvider)
            .read_file_if_exists(project_path)
            .await
    }

    async fn read_dir(
        &self,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Vec<RawDirEntry>> {
        let project_path = self.resolve(path);
        let mut entries = (&self.io as &dyn IoProvider)
            .read_dir(project_path)
            .await
            .with_buck_error_context(|| format!("Error listing dir `{}`", path))?;

        // Make sure entries are deterministic, since read_dir isn't.
        entries.sort_by(|a, b| a.file_name.cmp(&b.file_name));

        Ok(entries)
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        let project_path = self.resolve(path);

        let Some(metadata) = (&self.io as &dyn IoProvider)
            .read_path_metadata_if_exists(project_path)
            .await
            .with_buck_error_context(|| format!("Error accessing metadata for path `{}`", path))?
        else {
            return Ok(None);
        };
        Ok(Some(metadata.try_map(
            |path| match path.strip_prefix_opt(&self.get_base_path()) {
                Some(path) => Ok(Arc::new(CellPath::new(self.cell, path.to_owned().into()))),
                None => Err(internal_error!(
                    "Non-cell internal symlink at `{}` in cell `{}`",
                    path,
                    self.cell
                )),
            },
        )?))
    }

    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::always_false()
    }
}

pub(crate) async fn get_file_ops_delegate(
    ctx: &mut DiceComputations<'_>,
    cell: CellName,
    setup: GitCellSetup,
) -> buck2_error::Result<Arc<GitFileOpsDelegate>> {
    #[derive(
        dupe::Dupe,
        Clone,
        Debug,
        derive_more::Display,
        PartialEq,
        Eq,
        Hash,
        allocative::Allocative
    )]
    #[display("({}, {})", _0, _1)]
    struct GitFileOpsDelegateKey(CellName, GitCellSetup);

    #[async_trait::async_trait]
    impl Key for GitFileOpsDelegateKey {
        type Value = buck2_error::Result<Arc<GitFileOpsDelegate>>;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            cancellations: &CancellationContext,
        ) -> Self::Value {
            let artifact_fs = ctx.get_artifact_fs().await?;
            let ops = GitFileOpsDelegate {
                buck_out_resolver: artifact_fs.buck_out_path_resolver().clone(),
                cell: self.0,
                setup: self.1.dupe(),
                io: FsIoProvider::new(
                    artifact_fs.fs().dupe(),
                    ctx.global_data().get_digest_config().cas_digest_config(),
                ),
            };
            download_and_materialize(ctx, &ops.get_base_path(), &self.1, cancellations).await?;
            Ok(Arc::new(ops))
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            false
        }
    }

    ctx.compute(&GitFileOpsDelegateKey(cell, setup)).await?
}

pub(crate) async fn materialize_all(
    ctx: &mut DiceComputations<'_>,
    cell: CellName,
    setup: GitCellSetup,
) -> buck2_error::Result<ProjectRelativePathBuf> {
    // Get the `GitFileOpsDelegate` instance to make sure all the data is materialized.
    let ops = get_file_ops_delegate(ctx, cell, setup.dupe()).await?;
    Ok(ops.get_base_path())
}
