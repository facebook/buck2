/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_common::file_ops::metadata::FileMetadata;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::directory_iterator::DirectoryIteratorPathStack;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_directory::directory::walk::unordered_entry_walk;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::clean_output_paths::CleanOutputPaths;
use buck2_execute::execute::clean_output_paths::cleanup_path;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use buck2_execute::materialize::materializer::WriteRequest;
use buck2_execute::materialize::utils::dynamic_priority_handle::DynamicPriorityHandle;
use buck2_execute::materialize::utils::priority_semaphore::Priority;
use buck2_execute::re::manager::ReConnectionManager;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use gazebo::prelude::*;
use remote_execution::NamedDigest;
use remote_execution::NamedDigestWithPermissions;

use crate::materializers::io::MaterializeTreeStructure;

/// Serialization of concurrent writers to one output path.
///
/// Windows-only. Deleting a file that another handle still has open marks it
/// delete-pending rather than removing it, and a delete-pending name can be
/// neither opened nor stat'd, so two writers of the same content-based output
/// path fail with `Access is denied`. POSIX unlink has no such state, and a
/// content-based path means every writer writes identical bytes, so elsewhere
/// the writes race harmlessly and converge.
#[cfg(windows)]
mod path_lock {
    use std::hash::DefaultHasher;
    use std::hash::Hash;
    use std::hash::Hasher;
    use std::sync::LazyLock;
    use std::sync::Mutex;
    use std::sync::MutexGuard;

    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    #[cfg(test)]
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_util::threads::available_parallelism;

    /// Stripes per unit of host parallelism. Collisions between unrelated paths
    /// fall as roughly `1 / (2 * STRIPES_PER_THREAD)`, so 16 keeps about 3% of
    /// concurrent writers waiting on a lock they have no reason to share, for a
    /// table of `16 * parallelism` empty mutexes — a few thousand, tens of
    /// kilobytes, on a large host.
    const STRIPES_PER_THREAD: usize = 16;

    /// Which of `stripes` locks serializes writes to `path`.
    fn stripe_index(path: &ProjectRelativePath, stripes: usize) -> usize {
        let mut hasher = DefaultHasher::new();
        path.hash(&mut hasher);
        (hasher.finish() % stripes as u64) as usize
    }

    /// Serializes writes to a single output path.
    ///
    /// Writers of unrelated paths that hash to one stripe serialize with no
    /// benefit, so the table is sized off host parallelism. `DirectIoExecutor`,
    /// which Windows uses, applies no concurrency cap of its own.
    pub(super) fn write_lock(path: &ProjectRelativePath) -> MutexGuard<'static, ()> {
        // TODO(jtbraun): see if we can enable posix file delete semantics here for windows, and back out these locks

        static LOCKS: LazyLock<Vec<Mutex<()>>> = LazyLock::new(|| {
            (0..available_parallelism() * STRIPES_PER_THREAD)
                .map(|_| Mutex::new(()))
                .collect()
        });

        let lock = &LOCKS[stripe_index(path, LOCKS.len())];
        // The lock guards no data, so there is no state a panicking writer could
        // have left inconsistent.
        lock.lock().unwrap_or_else(|e| e.into_inner())
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        /// A buck-out path shaped like the ones the materializer actually writes:
        /// long, sharing a prefix, differing late.
        fn out_path(i: usize) -> ProjectRelativePathBuf {
            ProjectRelativePathBuf::try_from(format!(
                "buck-out/v2/gen/root/cfg0123456789abcdef/__target_{i}__/argsfile.json"
            ))
            .unwrap()
        }

        #[test]
        fn test_stripe_index_spreads_paths_evenly() {
            const STRIPES: usize = 64;
            const PATHS: usize = 10_000;

            let mut load = vec![0usize; STRIPES];
            for i in 0..PATHS {
                load[stripe_index(&out_path(i), STRIPES)] += 1;
            }

            let mean = PATHS / STRIPES;
            let max = *load.iter().max().unwrap();
            assert!(load.iter().all(|&n| n > 0), "some stripe went unused");
            assert!(
                max < mean * 2,
                "hottest stripe has {max} paths, mean is {mean}"
            );
        }

        /// How often two writers of *unrelated* paths land on one stripe and
        /// serialize needlessly. This is the only way striping can cost anything;
        /// same-path writers are meant to serialize.
        #[test]
        fn test_unrelated_paths_rarely_share_a_stripe() {
            for parallelism in [8usize, 32, 64] {
                // Production sizes the table off host parallelism.
                let stripes = parallelism * STRIPES_PER_THREAD;
                let batches = 200;

                let mut waiters = 0usize;
                for b in 0..batches {
                    let mut load = vec![0usize; stripes];
                    for w in 0..parallelism {
                        load[stripe_index(&out_path(b * parallelism + w), stripes)] += 1;
                    }
                    // Whoever holds the stripe first does not wait; the rest do.
                    waiters += load
                        .iter()
                        .filter(|&&n| n > 1)
                        .map(|&n| n - 1)
                        .sum::<usize>();
                }

                let pct = waiters * 100 / (batches * parallelism);
                assert!(
                    pct <= 5,
                    "at parallelism {parallelism}, {pct}% of writers wait on an unrelated path"
                );
            }
        }
    }
}

#[cfg(windows)]
use path_lock::write_lock;

#[cfg(not(windows))]
fn write_lock(_path: &ProjectRelativePath) {}

/// Replace whatever is at `path` with `content`.
///
/// Serialized against concurrent writers of the same path, which a target
/// analyzed under several configurations produces for one content-based output.
/// Every writer of a materialized file must go through here: on Windows two
/// unsynchronized writers race on delete-pending state and fail with `Access is
/// denied`.
pub(crate) fn locked_write(
    fs: &ProjectRoot,
    path: &ProjectRelativePath,
    content: &[u8],
    is_executable: bool,
) -> buck2_error::Result<()> {
    let _guard = write_lock(path);
    cleanup_path(fs, path)?;
    fs.write_file(path, content, is_executable)?;
    Ok(())
}

pub async fn write_to_disk<'a>(
    fs: &ProjectRoot,
    io_executor: &dyn BlockingExecutor,
    digest_config: DigestConfig,
    generate: Box<dyn FnOnce() -> buck2_error::Result<Vec<WriteRequest>> + Send + 'a>,
) -> buck2_error::Result<Vec<ArtifactValue>> {
    io_executor
        .execute_io_inline({
            move || {
                let requests = generate()?;
                let mut values = Vec::with_capacity(requests.len());

                for WriteRequest {
                    path,
                    content,
                    is_executable,
                    configuration_path: _,
                } in requests
                {
                    let digest = TrackedFileDigest::from_content(
                        &content,
                        digest_config.cas_digest_config(),
                    );

                    locked_write(fs, &path, &content, is_executable)?;

                    values.push(ArtifactValue::file(FileMetadata {
                        digest,
                        is_executable,
                    }));
                }

                Ok(values)
            }
        })
        .await
}

pub async fn cas_download(
    fs: &ProjectRoot,
    io: &dyn BlockingExecutor,
    re: &ReConnectionManager,
    info: &CasDownloadInfo,
    artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    cancellations: &CancellationContext,
) -> buck2_error::Result<()> {
    io.execute_io(
        Box::new(CleanOutputPaths {
            paths: artifacts.map(|(p, _)| p.to_owned()),
        }),
        cancellations,
    )
    .await?;

    for (path, value) in artifacts.iter() {
        io.execute_io(
            Box::new(MaterializeTreeStructure {
                path: path.to_owned(),
                entry: value.entry().dupe(),
            }),
            cancellations,
        )
        .await?;
    }

    let mut files = Vec::new();
    for (path, value) in artifacts.iter() {
        let mut walk = unordered_entry_walk(value.entry().as_ref().map_dir(Directory::as_ref));
        while let Some((entry_path, entry)) = walk.next() {
            if let DirectoryEntry::Leaf(ActionDirectoryMember::File(m)) = entry {
                files.push(NamedDigestWithPermissions {
                    named_digest: NamedDigest {
                        digest: m.digest.to_re(),
                        name: fs
                            .resolve(path.join(entry_path.get()))
                            .as_maybe_relativized_str()?
                            .to_owned(),
                        ..Default::default()
                    },
                    is_executable: m.is_executable,
                    ..Default::default()
                });
            }
        }
    }

    let re_conn = re.get_re_connection();
    let re_client = re_conn.get_client().with_use_case(info.re_use_case);
    cancellations
        .critical_section(|| {
            re_client.materialize_files(files, DynamicPriorityHandle::new(Priority::High), info)
        })
        .await?;
    Ok(())
}
