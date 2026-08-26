/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::io::Write;

use buck2_common::file_ops::metadata::FileMetadata;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_core::fs::buck_out_path::BuckOutPathKind;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::directory_iterator::DirectoryIteratorPathStack;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_directory::directory::walk::unordered_entry_walk;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
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
use buck2_fs::error::IoResultExt;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_path::AbsPath;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use gazebo::prelude::*;
use remote_execution::NamedDigest;
use remote_execution::NamedDigestWithPermissions;

use crate::materializers::io::MaterializeTreeStructure;

/// Serialization of concurrent writers to one output path.
///
/// Content-based-paths may have more than one writer trying to write the same
/// content at the same time, one per configuration the owning target is
/// analyzed under. Serializing them matters everywhere because the fallback
/// path in `maybe_locked_write` needs to be able to remove and rewrite the
/// files once to guarantee a normal file is in place. The serialization there
/// guarantees the second writer uses the atomic-rename path, instead of acting
/// on a stale "this path is obstructed" and removing what the first writer just
/// published. Allowing any writer but the first to remove the file risks that
/// the action using the file as an input downstream of the first may have the
/// file removed out from under it.  On Windows it additionally covers the fast
/// path: deleting or replacing a file whose name another handle still has open
/// marks it delete-pending rather than removing it, and a delete-pending name
/// can be neither opened nor stat'd, so two unsynchronized writers fail with
/// `Access is denied`.
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

use path_lock::write_lock;

/// Replace whatever is at `path` with `content`, for declare-time
/// (non-deferred) writes. Deferred writes are serialized per-path by the
/// materializer and need none of this.
///
/// A consumer of a completed same-path write may already be reading the file
/// while another writer republishes it (see `path_lock`), so a plain file at
/// `path` is only ever *replaced* — atomically, by rename — never removed or
/// truncated in place.
pub(crate) fn maybe_locked_write(
    fs: &ProjectRoot,
    path: &ProjectRelativePath,
    content: &[u8],
    is_executable: bool,
    path_kind: BuckOutPathKind,
) -> buck2_error::Result<()> {
    let _guard = match path_kind {
        BuckOutPathKind::ContentHash => Some(write_lock(path)),
        BuckOutPathKind::Configuration => None,
    };

    let Err(first_attempt) = write_via_atomic_rename(fs, path, content, is_executable) else {
        return Ok(());
    };

    match fs_util::symlink_metadata(fs.resolve(path)) {
        // A plain file here is either a completed publish (readers may
        // depend on it) or stale content nothing can be reading; either way
        // removing it cannot rescue an environmental failure, so report the
        // error and touch nothing.
        Ok(m) if m.is_file() => Err(first_attempt),
        // No observable plain file means no completed publish and so no
        // reader: safe to clear whatever is here — including a stale file
        // in a parent position, which fails the stat itself with `ENOTDIR`
        // — and publish atomically. A stat that failed for access reasons
        // instead fails `cleanup_path` on the same barrier before anything
        // is removed.
        _ => {
            tracing::warn!(
                path = %path,
                "Write destination obstructed or unreadable; repairing"
            );
            cleanup_path(fs, path)?;
            write_via_atomic_rename(fs, path, content, is_executable)
        }
    }
}

/// Writes `content` to a freshly created temporary sibling of `path`, then
/// renames it into place, atomically replacing any plain file already there.
///
/// `tempfile` owns temp naming (random, claimed with create-new semantics, so
/// nothing that already exists is ever touched) and deletes the temp on drop
/// when the publish does not complete; the publish rename goes through
/// `fs_util` so the operation with replace semantics stays in buck's IO
/// layer. The temp creation and content write bypass `fs_util` (no IO
/// counters, retries, or error categorization for those two operations).
fn write_via_atomic_rename(
    fs: &ProjectRoot,
    path: &ProjectRelativePath,
    content: &[u8],
    is_executable: bool,
) -> buck2_error::Result<()> {
    let (Some(parent), Some(file_name)) = (path.parent(), path.file_name()) else {
        return Err(internal_error!(
            "write path `{path}` has no parent directory"
        ));
    };
    let dest_dir = fs.resolve(parent);
    fs_util::create_dir_all(&dest_dir)?;

    // The destination's name in the prefix is only for debuggability. Cap it
    // (on a char boundary) so the temp name stays under NAME_MAX even when
    // the destination name is itself near the limit.
    const TEMP_PREFIX_NAME_BYTES: usize = 64;
    let name = file_name.as_str();
    let mut cap = name.len().min(TEMP_PREFIX_NAME_BYTES);
    while !name.is_char_boundary(cap) {
        cap -= 1;
    }
    let prefix = format!(".{}.", &name[..cap]);
    let mut builder = tempfile::Builder::new();
    builder.prefix(&prefix).suffix(".tmp");
    // The rename carries the temp's mode onto the destination, so give the
    // temp the mode a materialized output is expected to have: umask-defaulted
    // 0666 (`File::create` semantics), not tempfile's owner-only default. The
    // executable bit is applied below.
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        builder.permissions(std::fs::Permissions::from_mode(0o666));
    }
    let mut temp = builder
        .tempfile_in(&dest_dir)
        .with_buck_error_context(|| format!("creating temp file for `{path}`"))?;
    temp.write_all(content)
        .with_buck_error_context(|| format!("writing temp file for `{path}`"))?;

    let temp_path = temp.into_temp_path();
    let temp_abs = AbsPath::new(&*temp_path)?;
    #[cfg(unix)]
    if is_executable {
        use std::os::unix::fs::PermissionsExt;
        fs_util::set_permissions(temp_abs, std::fs::Permissions::from_mode(0o755))
            .categorize_internal()?;
    }
    #[cfg(not(unix))]
    let _ = is_executable;

    fs_util::rename(temp_abs, fs.resolve(path)).categorize_internal()?;
    // The rename freed the temp name for another writer to claim, so
    // `temp_path`'s delete-on-drop must be disarmed — it deletes by name and
    // could unlink someone else's fresh claim. (On failure above, that drop
    // is exactly what cleans our temp up.)
    // `-W let-underscore-drop` wants a named binding for the dropped Result.
    let _unused = temp_path.keep();
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
                    path_kind,
                    configuration_path: _,
                } in requests
                {
                    let digest = TrackedFileDigest::from_content(
                        &content,
                        digest_config.cas_digest_config(),
                    );

                    maybe_locked_write(fs, &path, &content, is_executable, path_kind)?;

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

// On Windows a reader's open handle (opened without `FILE_SHARE_DELETE`) can
// legitimately fail a concurrent replace, so continuous readability is only
// promised on Unix.
#[cfg(all(test, unix))]
mod tests {
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::time::Duration;
    use std::time::Instant;

    use buck2_core::fs::buck_out_path::BuckOutPathKind;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_fs::error::IoResultExt;
    use buck2_fs::fs_util;

    use crate::materializers::immediate::maybe_locked_write;

    /// A target analyzed under several configurations produces one writer per
    /// configuration for a single content-based output path, all writing the
    /// same bytes, while consumers of an earlier writer's completed action
    /// read the file. Readers must never observe the path missing or partially
    /// written.
    #[test]
    fn test_rewrites_never_expose_a_missing_file() -> buck2_error::Result<()> {
        let project = ProjectRootTemp::new()?;
        let path = ProjectRelativePath::new("gen/__t__/0a1b/artifacts.json")?;
        let content = b"[\"some plausible artifact contents\"]\n";
        maybe_locked_write(
            project.path(),
            path,
            content,
            false,
            BuckOutPathKind::ContentHash,
        )?;

        let stop = AtomicBool::new(false);
        let abs = project.path().resolve(path);
        let mut reader_failure = None;
        std::thread::scope(|scope| {
            for _ in 0..2 {
                scope.spawn(|| {
                    while !stop.load(Ordering::Relaxed) {
                        if maybe_locked_write(
                            project.path(),
                            path,
                            content,
                            false,
                            BuckOutPathKind::ContentHash,
                        )
                        .is_err()
                        {
                            // Surfaced by the reader below as a missing or
                            // corrupt file, or benign if the reader's window
                            // has already passed.
                            break;
                        }
                    }
                });
            }

            let deadline = Instant::now() + Duration::from_millis(500);
            while Instant::now() < deadline {
                match fs_util::read(&abs) {
                    Ok(read) if read == content => {}
                    other => {
                        reader_failure = Some(format!("{other:?}"));
                        break;
                    }
                }
            }
            stop.store(true, Ordering::Relaxed);
        });

        assert!(
            reader_failure.is_none(),
            "reader must always see the full file while writers republish it, got: {}",
            reader_failure.unwrap(),
        );

        let leftovers: Vec<String> = fs_util::read_dir(abs.parent().unwrap())
            .categorize_internal()?
            .map(|entry| Ok(entry?.file_name().to_string_lossy().into_owned()))
            .collect::<buck2_error::Result<_>>()?;
        assert_eq!(
            leftovers,
            vec!["artifacts.json".to_owned()],
            "completed writers must leave only the artifact behind, no temp files"
        );
        Ok(())
    }

    /// Stale disk state can leave a directory at a path the current build
    /// publishes as a file — routinely for configuration-based paths (a rule
    /// change turned yesterday's directory output into today's file), and
    /// only via corruption or outside interference for content-based paths
    /// (the hash in the path makes legitimate collisions near-impossible).
    /// This test exercises the harder, content-based shape: repair must be
    /// single-flight, and once any writer has published, readers see the
    /// file continuously even while the writer that lost the repair race
    /// retries.
    #[test]
    fn test_directory_obstruction_repair_keeps_readers_whole() -> buck2_error::Result<()> {
        let project = ProjectRootTemp::new()?;
        let path = ProjectRelativePath::new("gen/__t__/0a1b/artifacts.json")?;
        let content = b"[\"some plausible artifact contents\"]\n";
        let abs = project.path().resolve(path);
        fs_util::create_dir_all(&abs)?;
        fs_util::write(
            project.path().resolve(ProjectRelativePath::new(
                "gen/__t__/0a1b/artifacts.json/stale",
            )?),
            b"stale junk from a previous build",
        )
        .categorize_internal()?;

        let stop = AtomicBool::new(false);
        let mut failure = None;
        std::thread::scope(|scope| {
            for _ in 0..2 {
                scope.spawn(|| {
                    while !stop.load(Ordering::Relaxed) {
                        if maybe_locked_write(
                            project.path(),
                            path,
                            content,
                            false,
                            BuckOutPathKind::ContentHash,
                        )
                        .is_err()
                        {
                            break;
                        }
                    }
                });
            }

            let deadline = Instant::now() + Duration::from_millis(500);
            let mut published = false;
            while Instant::now() < deadline {
                match fs_util::read(&abs) {
                    Ok(read) if read == content => published = true,
                    // Reads fail while the obstruction is still in place;
                    // after the first publish the file must never regress.
                    Err(_) | Ok(_) if !published => {}
                    other => {
                        failure = Some(format!("{other:?}"));
                        break;
                    }
                }
            }
            stop.store(true, Ordering::Relaxed);
            if !published && failure.is_none() {
                failure = Some("no writer ever repaired the obstruction".to_owned());
            }
        });

        assert!(
            failure.is_none(),
            "readers must see the file continuously once the obstruction is repaired, got: {}",
            failure.unwrap(),
        );
        Ok(())
    }

    /// An environmental write failure must not remove a published file: the
    /// losing writer reports its error and leaves the destination alone.
    #[test]
    fn test_failed_rewrite_does_not_destroy_the_destination() -> buck2_error::Result<()> {
        use std::os::unix::fs::PermissionsExt;

        let project = ProjectRootTemp::new()?;
        let path = ProjectRelativePath::new("gen/__t__/0a1b/artifacts.json")?;
        let content = b"[\"some plausible artifact contents\"]\n";
        maybe_locked_write(
            project.path(),
            path,
            content,
            false,
            BuckOutPathKind::ContentHash,
        )?;

        let abs = project.path().resolve(path);
        let dir = abs.parent().unwrap();
        // A read-only directory makes the temp file creation fail while the
        // published destination stays a perfectly healthy file.
        fs_util::set_permissions(dir, std::fs::Permissions::from_mode(0o555))
            .categorize_internal()?;

        // Root (and other DAC-bypassing environments) ignore the mode bits,
        // making the injection inert; probe for that and skip rather than
        // assert a failure the environment cannot produce.
        let probe = project
            .path()
            .resolve(ProjectRelativePath::new("gen/__t__/0a1b/.dac-probe")?);
        if fs_util::write(&probe, b"probe").is_ok() {
            fs_util::set_permissions(dir, std::fs::Permissions::from_mode(0o755))
                .categorize_internal()?;
            return Ok(());
        }

        let result = maybe_locked_write(
            project.path(),
            path,
            content,
            false,
            BuckOutPathKind::ContentHash,
        );
        fs_util::set_permissions(dir, std::fs::Permissions::from_mode(0o755))
            .categorize_internal()?;

        assert!(
            result.is_err(),
            "the failed writer must report its error, not swallow it"
        );
        assert_eq!(
            fs_util::read(&abs).categorize_internal()?,
            content,
            "a published file must survive another writer's environmental failure"
        );
        Ok(())
    }

    /// A destination name near NAME_MAX must still publish: the temp name
    /// only embeds a capped portion of it.
    #[test]
    fn test_near_name_max_destination_publishes() -> buck2_error::Result<()> {
        let project = ProjectRootTemp::new()?;
        let name = "x".repeat(250);
        let path = ProjectRelativePathBuf::try_from(format!("gen/__t__/0a1b/{name}"))?;
        let content = b"[\"some plausible artifact contents\"]\n";

        maybe_locked_write(
            project.path(),
            &path,
            content,
            false,
            BuckOutPathKind::ContentHash,
        )?;

        assert_eq!(
            fs_util::read(project.path().resolve(&path)).categorize_internal()?,
            content,
            "a legal destination name must publish regardless of its length"
        );
        Ok(())
    }

    /// A stale file in a *parent* position of the destination must be
    /// repaired (the T85589819 shape `cleanup_path`'s parent walk exists
    /// for). Statting the destination itself fails with `ENOTDIR` here,
    /// which must route to the repair branch, not out of the writer.
    #[test]
    fn test_file_in_parent_position_is_repaired() -> buck2_error::Result<()> {
        let project = ProjectRootTemp::new()?;
        let path = ProjectRelativePath::new("gen/__t__/0a1b/artifacts.json")?;
        let content = b"[\"some plausible artifact contents\"]\n";

        let parent_file = project
            .path()
            .resolve(ProjectRelativePath::new("gen/__t__/0a1b")?);
        fs_util::create_dir_all(parent_file.parent().unwrap())?;
        fs_util::write(&parent_file, b"stale file where a directory belongs")
            .categorize_internal()?;

        maybe_locked_write(
            project.path(),
            path,
            content,
            false,
            BuckOutPathKind::Configuration,
        )?;

        assert_eq!(
            fs_util::read(project.path().resolve(path)).categorize_internal()?,
            content,
            "a stale file in a parent position must be repaired and the write published"
        );
        Ok(())
    }
}
