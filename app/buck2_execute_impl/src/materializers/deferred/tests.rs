/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;

use buck2_common::file_ops::metadata::FileMetadata;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_error::internal_error;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::insert_file;
use buck2_execute::materialize::materializer::DeferredMaterializerSubscription;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use parking_lot::Mutex;

use super::*;

#[test]
fn test_find_artifacts() -> buck2_error::Result<()> {
    let artifact1 = ProjectRelativePathBuf::unchecked_new("foo/bar/baz".to_owned());
    let artifact2 = ProjectRelativePathBuf::unchecked_new("foo/bar/bar/qux".to_owned());
    let artifact3 = ProjectRelativePathBuf::unchecked_new("foo/bar/bar/quux".to_owned());
    let artifact4 = ProjectRelativePathBuf::unchecked_new("foo/bar/qux/quuz".to_owned());
    let non_artifact1 = ProjectRelativePathBuf::unchecked_new("foo/bar/qux".to_owned());
    let non_artifact2 = ProjectRelativePathBuf::unchecked_new("foo/bar/bar/corge".to_owned());

    let file = FileMetadata::empty(DigestConfig::testing_default().cas_digest_config());

    // Build deps with artifacts 1-3, and non-artifacts 1-2
    let mut builder = ActionDirectoryBuilder::empty();
    insert_file(
        &mut builder,
        artifact1.join(ForwardRelativePath::new("f1").unwrap()),
        file.dupe(),
    )?;
    insert_file(
        &mut builder,
        artifact2.join(ForwardRelativePath::new("d/f1").unwrap()),
        file.dupe(),
    )?;
    insert_file(&mut builder, artifact3.clone(), file.dupe())?;
    insert_file(&mut builder, non_artifact2, file.dupe())?;
    builder.mkdir(&non_artifact1)?;

    // Build tree with artifacts 1-4
    let mut tree: FileTree<()> = FileTree::new();
    tree.insert(artifact1.iter().map(|f| f.to_owned()), ());
    tree.insert(artifact2.iter().map(|f| f.to_owned()), ());
    tree.insert(artifact3.iter().map(|f| f.to_owned()), ());
    tree.insert(artifact4.iter().map(|f| f.to_owned()), ());

    let expected_artifacts: HashSet<_> =
        vec![artifact1, artifact2, artifact3].into_iter().collect();
    let found_artifacts: HashSet<_> = tree.find_artifacts(&builder).into_iter().collect();
    assert_eq!(found_artifacts, expected_artifacts);
    Ok(())
}

#[test]
fn test_remove_path() {
    fn insert(tree: &mut FileTree<String>, path: &str) {
        tree.insert(
            ProjectRelativePath::unchecked_new(path)
                .iter()
                .map(|f| f.to_owned()),
            path.to_owned(),
        );
    }

    let mut tree: FileTree<String> = FileTree::new();
    insert(&mut tree, "a/b/c/d");
    insert(&mut tree, "a/b/c/e");
    insert(&mut tree, "a/c");

    let removed_subtree = tree.remove_path(ProjectRelativePath::unchecked_new("a/b"));
    // Convert to HashMap<String, String> so it's easier to test
    let removed_subtree: HashMap<String, String> = removed_subtree
        .map(|(k, v)| (k.as_str().to_owned(), v))
        .collect();

    assert_eq!(removed_subtree.len(), 2);
    assert_eq!(removed_subtree.get("a/b/c/d"), Some(&"a/b/c/d".to_owned()));
    assert_eq!(removed_subtree.get("a/b/c/e"), Some(&"a/b/c/e".to_owned()));
}

#[cfg(test)]
mod state_machine {
    use std::path::Path;
    use std::sync::Barrier;
    use std::thread;

    use assert_matches::assert_matches;
    use buck2_common::file_ops::metadata::Symlink;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_error::BuckErrorContext;
    use buck2_error::buck2_error;
    use buck2_events::daemon_id::DaemonId;
    use buck2_events::source::ChannelEventSource;
    use buck2_execute::directory::ActionDirectoryEntry;
    use buck2_execute::directory::ActionSharedDirectory;
    use buck2_execute::directory::INTERNER;
    use buck2_execute::execute::blocking::IoRequest;
    use buck2_fs::fs_util::ReadDir;
    use buck2_fs::fs_util::uncategorized as fs_util;
    use buck2_fs::paths::RelativePathBuf;
    use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
    use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
    use buck2_util::threads::ignore_stack_overflow_checks_for_future;
    use buck2_wrapper_common::invocation_id::TraceId;
    use futures::StreamExt;
    use futures::future::BoxFuture;
    use futures::future::FutureExt;
    use tokio::time::Duration as TokioDuration;
    use tokio::time::sleep;

    use super::*;
    use crate::materializers::deferred::clean_stale::CleanInvalidatedPathRequest;
    use crate::materializers::deferred::command_processor::TestingDeferredMaterializerCommandProcessor;
    use crate::materializers::deferred::subscriptions::MaterializerSubscriptionOperation;
    use crate::materializers::deferred::subscriptions::SubscriptionHandle;
    use crate::sqlite::materializer_db::testing_materializer_state_sqlite_db;

    #[derive(Debug, Eq, PartialEq, Allocative)]
    enum Op {
        Clean,
        Materialize,
        MaterializeError,
    }

    #[derive(Allocative)]
    struct StubIoHandler {
        log: Mutex<Vec<(Op, ProjectRelativePathBuf)>>,
        fail: Mutex<bool>,
        fail_paths: Mutex<Vec<ProjectRelativePathBuf>>,
        // If set, add a sleep when materializing to simulate a long materialization period
        materialization_config: HashMap<ProjectRelativePathBuf, TokioDuration>,
        #[allocative(skip)]
        read_dir_barriers: Option<Arc<(Barrier, Barrier)>>,
        #[allocative(skip)]
        clean_barriers: Option<Arc<(Barrier, Barrier)>>,
        digest_config: DigestConfig,
        buck_out_path: ProjectRelativePathBuf,
        fs: ProjectRoot,
    }

    impl DeferredMaterializerAccessor<StubIoHandler> {
        // Ensure that the command thread ends so that the command processor is dropped,
        // and the sqlite connection is flushed and closed.
        // Needed since the default destructor assumes the process is about to die and shouldn't need to block.
        fn abort(mut self) {
            self.command_sender
                .send(MaterializerCommand::Abort)
                .unwrap();
            self.command_thread.take().unwrap().join().unwrap();
        }
    }

    impl StubIoHandler {
        fn take_log(&self) -> Vec<(Op, ProjectRelativePathBuf)> {
            std::mem::take(&mut *self.log.lock())
        }

        fn set_fail(&self, fail: bool) {
            *self.fail.lock() = fail;
        }

        fn set_fail_on(&self, paths: Vec<ProjectRelativePathBuf>) {
            *self.fail_paths.lock() = paths;
        }

        pub fn new(fs: ProjectRoot) -> Self {
            Self {
                log: Default::default(),
                fail: Default::default(),
                fail_paths: Default::default(),
                materialization_config: HashMap::new(),
                read_dir_barriers: None,
                clean_barriers: None,
                digest_config: DigestConfig::testing_default(),
                buck_out_path: make_path("buck-out/v2"),
                fs,
            }
        }

        pub fn with_materialization_config(
            mut self,
            materialization_config: HashMap<ProjectRelativePathBuf, TokioDuration>,
        ) -> Self {
            self.materialization_config = materialization_config;
            self
        }

        pub fn with_read_dir_barriers(
            mut self,
            read_dir_barriers: Arc<(Barrier, Barrier)>,
        ) -> Self {
            self.read_dir_barriers = Some(read_dir_barriers);
            self
        }

        pub fn with_clean_barriers(mut self, clean_barriers: Arc<(Barrier, Barrier)>) -> Self {
            self.clean_barriers = Some(clean_barriers);
            self
        }
    }

    impl StubIoHandler {
        fn actually_write(self: &Arc<Self>, path: &ProjectRelativePathBuf, write: &Arc<WriteFile>) {
            let data = zstd::bulk::decompress(&write.compressed_data, write.decompressed_size)
                .buck_error_context("Error decompressing data")
                .unwrap();
            self.fs.write_file(path, data, write.is_executable).unwrap();
        }
    }

    #[async_trait]
    impl IoHandler for StubIoHandler {
        fn write<'a>(
            self: &Arc<Self>,
            path: ProjectRelativePathBuf,
            write: Arc<WriteFile>,
            version: Version,
            command_sender: Arc<MaterializerSender<Self>>,
            _cancellations: &'a CancellationContext,
        ) -> BoxFuture<'a, Result<(), SharedMaterializingError>> {
            self.actually_write(&path, &write);
            async move {
                let _ignored = command_sender.send_low_priority(
                    LowPriorityMaterializerCommand::MaterializationFinished {
                        path,
                        timestamp: Utc::now(),
                        version,
                        result: Ok(()),
                    },
                );
                Ok(())
            }
            .boxed()
        }

        async fn immediate_write<'a>(
            self: &Arc<Self>,
            _gen: Box<dyn FnOnce() -> buck2_error::Result<Vec<WriteRequest>> + Send + 'a>,
        ) -> buck2_error::Result<Vec<ArtifactValue>> {
            unimplemented!()
        }

        fn clean_path<'a>(
            self: &Arc<Self>,
            path: ProjectRelativePathBuf,
            version: Version,
            command_sender: Arc<MaterializerSender<Self>>,
            _cancellations: &'a CancellationContext,
        ) -> BoxFuture<'a, Result<(), buck2_error::Error>> {
            self.log.lock().push((Op::Clean, path.clone()));

            async move {
                let _ignored = command_sender.send_low_priority(
                    LowPriorityMaterializerCommand::CleanupFinished {
                        path,
                        version,
                        result: Ok(()),
                    },
                );
                Ok(())
            }
            .boxed()
        }

        async fn clean_invalidated_path<'a>(
            self: &Arc<Self>,
            request: CleanInvalidatedPathRequest,
            _cancellations: &'a CancellationContext,
        ) -> buck2_error::Result<()> {
            if let Some(barriers) = self.clean_barriers.as_ref() {
                // Allow tests to advance here, execute something and then continue
                barriers.as_ref().0.wait();
                barriers.as_ref().1.wait();
            }
            Box::new(request).execute(&self.fs)
        }

        async fn materialize_entry(
            self: &Arc<Self>,
            path: ProjectRelativePathBuf,
            _method: Arc<ArtifactMaterializationMethod>,
            _entry: ActionDirectoryEntry<ActionSharedDirectory>,
            _event_dispatcher: EventDispatcher,
            _cancellations: &CancellationContext,
        ) -> Result<(), MaterializeEntryError> {
            // Simulate a non-immediate materialization if configured
            match self.materialization_config.get(&path) {
                Some(duration) => {
                    sleep(*duration).await;
                }
                None => (),
            }

            if (*self.fail_paths.lock()).contains(&path) || *self.fail.lock() {
                self.log.lock().push((Op::MaterializeError, path));
                Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::MaterializationError,
                    "Injected error"
                )
                .into())
            } else {
                match _method.as_ref() {
                    ArtifactMaterializationMethod::Write(write) => {
                        self.actually_write(&path, write);
                    }
                    _ => {}
                }
                self.log.lock().push((Op::Materialize, path));
                Ok(())
            }
        }

        fn create_ttl_refresh(
            self: &Arc<Self>,
            _tree: &ArtifactTree,
            _min_ttl: Duration,
        ) -> Option<BoxFuture<'static, buck2_error::Result<()>>> {
            unimplemented!()
        }

        fn read_dir(&self, path: &AbsNormPathBuf) -> buck2_error::Result<ReadDir> {
            if let Some(barriers) = self.read_dir_barriers.as_ref() {
                // Allow tests to advance here, execute something and then continue
                barriers.as_ref().0.wait();
                barriers.as_ref().1.wait();
            }
            fs_util::read_dir(path)
        }

        fn buck_out_path(&self) -> &ProjectRelativePathBuf {
            &self.buck_out_path
        }

        fn re_client_manager(&self) -> &Arc<ReConnectionManager> {
            unimplemented!()
        }

        fn fs(&self) -> &ProjectRoot {
            &self.fs
        }

        fn digest_config(&self) -> DigestConfig {
            self.digest_config
        }
    }

    /// A stub command sender. We are calling materializer methods directly so that's all we need.
    fn channel() -> (
        Arc<MaterializerSender<StubIoHandler>>,
        MaterializerReceiver<StubIoHandler>,
    ) {
        // We don't use those counts in tests.
        static SENT: AtomicUsize = AtomicUsize::new(0);
        static RECEIVED: AtomicUsize = AtomicUsize::new(0);

        let (hi_send, hi_recv) = mpsc::unbounded_channel();
        let (lo_send, lo_recv) = mpsc::unbounded_channel();
        let counters = MaterializerCounters {
            sent: &SENT,
            received: &RECEIVED,
        };

        (
            Arc::new(MaterializerSender {
                high_priority: hi_send,
                low_priority: lo_send,
                counters,
                clean_guard: Default::default(),
            }),
            MaterializerReceiver {
                high_priority: hi_recv,
                low_priority: lo_recv,
                counters,
            },
        )
    }

    fn make_path(p: &str) -> ProjectRelativePathBuf {
        ProjectRelativePath::new(p).unwrap().to_owned()
    }

    fn temp_root() -> ProjectRoot {
        ProjectRootTemp::new().unwrap().path().clone()
    }

    async fn materialize_write(
        path: &ProjectRelativePathBuf,
        contents: &'static [u8],
        handle: &mut SubscriptionHandle<StubIoHandler>,
        dm: &DeferredMaterializerAccessor<StubIoHandler>,
    ) -> buck2_error::Result<()> {
        dm.declare_write(Box::new(|| {
            Ok(vec![WriteRequest {
                path: path.clone(),
                content: contents.to_vec(),
                is_executable: false,
            }])
        }))
        .await?;

        handle.subscribe_to_paths(vec![path.clone()]);

        dm.materialize_many(vec![path.clone()])
            .await?
            .next()
            .await
            .unwrap()?;
        // block until materialization_finished updates the tree
        handle.receiver().recv().await;
        Ok(())
    }

    fn make_db(fs: &ProjectRoot) -> (MaterializerStateSqliteDb, Option<MaterializerState>) {
        let (db, state) = testing_materializer_state_sqlite_db(
            fs,
            HashMap::from([("version".to_owned(), "0".to_owned())]),
            HashMap::new(),
            None,
        )
        .unwrap();
        (db, state.ok())
    }

    fn make_processor_for_io(
        io: Arc<StubIoHandler>,
    ) -> (
        DeferredMaterializerCommandProcessor<StubIoHandler>,
        Arc<MaterializerSender<StubIoHandler>>,
        MaterializerReceiver<StubIoHandler>,
        ChannelEventSource,
    ) {
        let (db, sqlite_state) = make_db(io.fs());
        let tree = ArtifactTree::initialize(sqlite_state);

        let (daemon_dispatcher_events, daemon_dispatcher_sink) =
            buck2_events::create_source_sink_pair();
        let daemon_dispatcher =
            EventDispatcher::new(TraceId::null(), DaemonId::new(), daemon_dispatcher_sink);

        let (command_sender, command_receiver) = channel();
        (
            DeferredMaterializerCommandProcessor::new(
                io,
                Some(db),
                Handle::current(),
                true,
                command_sender.dupe(),
                tree,
                CancellationContext::testing(),
                Arc::new(DeferredMaterializerStats::default()),
                Default::default(),
                true,
                daemon_dispatcher,
                true,
            ),
            command_sender,
            command_receiver,
            daemon_dispatcher_events,
        )
    }

    fn make_processor(
        materialization_config: HashMap<ProjectRelativePathBuf, TokioDuration>,
    ) -> (
        DeferredMaterializerCommandProcessor<StubIoHandler>,
        MaterializerReceiver<StubIoHandler>,
    ) {
        let (dm, _, receiver, _) = make_processor_for_io(Arc::new(
            StubIoHandler::new(temp_root()).with_materialization_config(materialization_config),
        ));
        (dm, receiver)
    }

    async fn make_materializer(
        io: Arc<StubIoHandler>,
        clean_stale_config: Option<CleanStaleConfig>,
    ) -> (
        DeferredMaterializerAccessor<StubIoHandler>,
        SubscriptionHandle<StubIoHandler>,
        ChannelEventSource,
    ) {
        let (mut processor, command_sender, command_receiver, daemon_dispatcher_events) =
            make_processor_for_io(io.dupe());

        let handle = {
            let (sender, recv) = oneshot::channel();
            MaterializerSubscriptionOperation::Create { sender }.execute(&mut processor);
            recv.await.unwrap()
        };

        let command_thread = thread_spawn("buck2-dm", {
            move || {
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .unwrap();

                rt.block_on(processor.run(
                    command_receiver,
                    TtlRefreshConfiguration {
                        frequency: std::time::Duration::default(),
                        min_ttl: chrono::Duration::zero(),
                        enabled: false,
                    },
                    0,
                    AccessTimesUpdates::Disabled,
                    clean_stale_config,
                ));
            }
        })
        .buck_error_context("Cannot start materializer thread")
        .unwrap();

        (
            DeferredMaterializerAccessor {
                command_thread: Some(command_thread),
                command_sender,
                materialize_final_artifacts: true,
                defer_write_actions: true,
                io,
                materializer_state_info: buck2_data::MaterializerStateInfo {
                    num_entries_from_sqlite: 0,
                },
                stats: Arc::new(DeferredMaterializerStats::default()),
            },
            handle,
            daemon_dispatcher_events,
        )
    }

    #[tokio::test]
    async fn test_declare_reuse() -> buck2_error::Result<()> {
        ignore_stack_overflow_checks_for_future(async {
            let (mut dm, _) = make_processor(Default::default());
            let digest_config = dm.io.digest_config();

            let path = make_path("foo/bar");
            let value = ArtifactValue::file(digest_config.empty_file());

            dm.testing_declare(&path, value.dupe());
            assert_eq!(dm.io.take_log(), &[(Op::Clean, path.clone())]);

            // When redeclaring the same artifact nothing happens.
            dm.testing_declare(&path, value.dupe());
            assert_eq!(dm.io.take_log(), &[]);

            let res = dm
                .materialize_artifact(&path, EventDispatcher::null())
                .ok_or_else(|| internal_error!("Expected a future"))?
                .await;
            assert_eq!(dm.io.take_log(), &[(Op::Materialize, path.clone())]);

            dm.testing_materialization_finished(path.clone(), Utc::now(), res);
            assert_eq!(dm.io.take_log(), &[]);

            // When redeclaring the same artifact nothing happens.
            dm.testing_declare(&path, value.dupe());
            assert_eq!(dm.io.take_log(), &[]);

            // When declaring the same artifact but under it, we clean it and it's a new artifact.
            let path2 = make_path("foo/bar/baz");
            dm.testing_declare(&path2, value.dupe());
            assert_eq!(dm.io.take_log(), &[(Op::Clean, path2.clone())]);

            let _ignore = dm
                .materialize_artifact(&path2, EventDispatcher::null())
                .ok_or_else(|| internal_error!("Expected a future"))?
                .await;
            assert_eq!(dm.io.take_log(), &[(Op::Materialize, path2.clone())]);

            Ok(())
        })
        .await
    }

    fn make_artifact_value_with_symlink_dep(
        target_path: &ProjectRelativePathBuf,
        target_from_symlink: &RelativePathBuf,
        digest_config: DigestConfig,
    ) -> buck2_error::Result<ArtifactValue> {
        let mut deps = ActionDirectoryBuilder::empty();
        let target = ActionDirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata::empty(
            digest_config.cas_digest_config(),
        )));
        deps.insert(target_path.as_forward_relative_path(), target)?;
        let symlink_value = ArtifactValue::new(
            ActionDirectoryEntry::Leaf(ActionDirectoryMember::Symlink(Arc::new(Symlink::new(
                target_from_symlink.clone(),
            )))),
            Some(
                deps.fingerprint(digest_config.as_directory_serializer())
                    .shared(&*INTERNER),
            ),
        );
        Ok(symlink_value)
    }

    #[tokio::test]
    async fn test_materialize_symlink_and_target() -> buck2_error::Result<()> {
        ignore_stack_overflow_checks_for_future(async {
            // Construct a tree with a symlink and its target, materialize both at once
            let symlink_path = make_path("foo/bar_symlink");
            let target_path = make_path("foo/bar_target");
            let target_from_symlink = RelativePathBuf::from_path(Path::new("bar_target"))?;

            let mut materialization_config = HashMap::new();
            // Materialize the symlink target slowly so that we actually hit the logic point where we
            // await for symlink targets and the entry materialization
            materialization_config.insert(target_path.clone(), TokioDuration::from_millis(100));

            let (mut dm, _) = make_processor(materialization_config);
            let digest_config = dm.io.digest_config();

            // Declare symlink target
            dm.testing_declare(
                &target_path,
                ArtifactValue::file(digest_config.empty_file()),
            );
            assert_eq!(dm.io.take_log(), &[(Op::Clean, target_path.clone())]);

            // Declare symlink
            let symlink_value = make_artifact_value_with_symlink_dep(
                &target_path,
                &target_from_symlink,
                digest_config,
            )?;
            dm.testing_declare(&symlink_path, symlink_value);
            assert_eq!(dm.io.take_log(), &[(Op::Clean, symlink_path.clone())]);

            dm.materialize_artifact(&symlink_path, EventDispatcher::null())
                .ok_or_else(|| internal_error!("Expected a future"))?
                .await
                .map_err(|_| {
                    buck2_error!(
                        buck2_error::ErrorTag::MaterializationError,
                        "error materializing"
                    )
                })?;

            let logs = dm.io.take_log();
            if cfg!(unix) {
                assert_eq!(
                    logs,
                    &[
                        (Op::Materialize, symlink_path.clone()),
                        (Op::Materialize, target_path.clone())
                    ]
                );
            } else {
                assert_eq!(
                    logs,
                    &[
                        (Op::Materialize, target_path.clone()),
                        (Op::Materialize, symlink_path.clone())
                    ]
                );
            }
            Ok(())
        })
        .await
    }

    #[tokio::test]
    async fn test_materialize_symlink_first_then_target() -> buck2_error::Result<()> {
        ignore_stack_overflow_checks_for_future(async {
            // Materialize a symlink, then materialize the target. Test that we still
            // materialize deps if the main artifact has already been materialized.
            let symlink_path = make_path("foo/bar_symlink");
            let target_path = make_path("foo/bar_target");
            let target_from_symlink = RelativePathBuf::from_path(Path::new("bar_target"))?;

            let mut materialization_config = HashMap::new();
            // Materialize the symlink target slowly so that we actually hit the logic point where we
            // await for symlink targets and the entry materialization
            materialization_config.insert(target_path.clone(), TokioDuration::from_millis(100));

            let (mut dm, _) = make_processor(materialization_config);
            let digest_config = dm.io.digest_config();

            // Declare symlink
            let symlink_value = make_artifact_value_with_symlink_dep(
                &target_path,
                &target_from_symlink,
                digest_config,
            )?;
            dm.testing_declare(&symlink_path, symlink_value);
            assert_eq!(dm.io.take_log(), &[(Op::Clean, symlink_path.clone())]);

            // Materialize the symlink, at this point the target is not in the tree so it's ignored
            let res = dm
                .materialize_artifact(&symlink_path, EventDispatcher::null())
                .ok_or_else(|| internal_error!("Expected a future"))?
                .await;

            let logs = dm.io.take_log();
            assert_eq!(logs, &[(Op::Materialize, symlink_path.clone())]);

            // Mark the symlink as materialized
            dm.testing_materialization_finished(symlink_path.clone(), Utc::now(), res);
            assert_eq!(dm.io.take_log(), &[]);

            // Declare symlink target
            dm.testing_declare(
                &target_path,
                ArtifactValue::file(digest_config.empty_file()),
            );
            assert_eq!(dm.io.take_log(), &[(Op::Clean, target_path.clone())]);

            // Materialize the symlink again.
            // This time, we don't re-materialize the symlink as that's already been done.
            // But we still materialize the target as that has not been materialized yet.
            dm.materialize_artifact(&symlink_path, EventDispatcher::null())
                .ok_or_else(|| internal_error!("Expected a future"))?
                .await
                .map_err(|_| {
                    buck2_error!(
                        buck2_error::ErrorTag::MaterializationError,
                        "error materializing"
                    )
                })?;

            let logs = dm.io.take_log();
            assert_eq!(logs, &[(Op::Materialize, target_path.clone())]);

            Ok(())
        })
        .await
    }

    #[tokio::test]
    async fn test_subscription_create_destroy() {
        let (mut dm, mut channel) = make_processor(Default::default());

        let handle = {
            let (sender, recv) = oneshot::channel();
            MaterializerSubscriptionOperation::Create { sender }.execute(&mut dm);
            recv.await.unwrap()
        };

        assert!(dm.subscriptions.has_subscription(&handle));

        drop(handle);

        while let Ok(cmd) = channel.high_priority.try_recv() {
            dm.testing_process_one_command(cmd);
        }

        assert!(!dm.subscriptions.has_any_subscriptions());
    }

    #[tokio::test]
    async fn test_subscription_notifications() {
        ignore_stack_overflow_checks_for_future(async {
            let (mut dm, mut channel) = make_processor(Default::default());
            let digest_config = dm.io.digest_config();
            let value = ArtifactValue::file(digest_config.empty_file());

            let mut handle = {
                let (sender, recv) = oneshot::channel();
                MaterializerSubscriptionOperation::Create { sender }.execute(&mut dm);
                recv.await.unwrap()
            };

            let foo_bar = make_path("foo/bar");
            let foo_bar_baz = make_path("foo/bar/baz");
            let bar = make_path("bar");
            let qux = make_path("qux");

            dm.testing_declare_existing(&foo_bar, value.dupe());

            handle.subscribe_to_paths(vec![foo_bar_baz.clone(), bar.clone()]);
            while let Ok(cmd) = channel.high_priority.try_recv() {
                dm.testing_process_one_command(cmd);
            }

            dm.testing_declare_existing(&bar, value.dupe());
            dm.testing_declare_existing(&foo_bar_baz, value.dupe());
            dm.testing_declare_existing(&qux, value.dupe());

            let mut paths = Vec::new();
            while let Ok(path) = handle.receiver().try_recv() {
                paths.push(path);
            }

            assert_eq!(paths, vec![foo_bar_baz.clone(), bar, foo_bar_baz]);
        })
        .await
    }

    #[tokio::test]
    async fn test_subscription_subscribe_also_materializes() -> buck2_error::Result<()> {
        ignore_stack_overflow_checks_for_future(async {
            let (mut dm, mut channel) = make_processor(Default::default());
            let digest_config = dm.io.digest_config();
            let value = ArtifactValue::file(digest_config.empty_file());

            let mut handle = {
                let (sender, recv) = oneshot::channel();
                MaterializerSubscriptionOperation::Create { sender }.execute(&mut dm);
                recv.await.unwrap()
            };

            let foo_bar = make_path("foo/bar");

            dm.testing_declare(&foo_bar, value.dupe());

            handle.subscribe_to_paths(vec![foo_bar.clone()]);
            while let Ok(cmd) = channel.high_priority.try_recv() {
                dm.testing_process_one_command(cmd);
            }

            // We need to yield to let the materialization task run. If we had a handle to it, we'd
            // just await it, but the subscription isn't retaining those handles.
            let mut log = Vec::new();
            while log.len() < 2 {
                log.extend(dm.io.take_log());
                tokio::task::yield_now().await;
            }

            assert_eq!(
                &log,
                &[
                    (Op::Clean, foo_bar.clone()),
                    (Op::Materialize, foo_bar.clone())
                ]
            );

            // Drain low priority commands. This should include our materialization finished message,
            // at which point we'll notify the subscription handle.
            while let Ok(cmd) = channel.low_priority.try_recv() {
                dm.testing_process_one_low_priority_command(cmd);
            }

            let mut paths = Vec::new();
            while let Ok(path) = handle.receiver().try_recv() {
                paths.push(path);
            }
            assert_eq!(paths, vec![foo_bar]);

            Ok(())
        })
        .await
    }

    #[tokio::test]
    async fn test_subscription_unsubscribe() {
        ignore_stack_overflow_checks_for_future(async {
            let (mut dm, mut channel) = make_processor(Default::default());
            let digest_config = dm.io.digest_config();
            let value1 = ArtifactValue::file(digest_config.empty_file());
            let value2 = ArtifactValue::dir(digest_config.empty_directory());

            let mut handle = {
                let (sender, recv) = oneshot::channel();
                MaterializerSubscriptionOperation::Create { sender }.execute(&mut dm);
                recv.await.unwrap()
            };

            let path = make_path("foo/bar");

            handle.subscribe_to_paths(vec![path.clone()]);
            while let Ok(cmd) = channel.high_priority.try_recv() {
                dm.testing_process_one_command(cmd);
            }

            dm.testing_declare_existing(&path, value1.dupe());

            handle.unsubscribe_from_paths(vec![path.clone()]);
            while let Ok(cmd) = channel.high_priority.try_recv() {
                dm.testing_process_one_command(cmd);
            }

            dm.sqlite_db
                .as_mut()
                .expect("db missing")
                .materializer_state_table()
                .delete(vec![path.clone()])
                .buck_error_context("delete failed")
                .unwrap();
            dm.testing_declare_existing(&path, value2.dupe());

            let mut paths = Vec::new();
            while let Ok(path) = handle.receiver().try_recv() {
                paths.push(path);
            }

            // Expect only one notification
            assert_eq!(paths, vec![path]);
        })
        .await
    }

    #[tokio::test]
    async fn test_invalidate_error() -> buck2_error::Result<()> {
        ignore_stack_overflow_checks_for_future(async{
            let (mut dm, _) = make_processor(Default::default());
            let digest_config = dm.io.digest_config();

            let path = make_path("test/invalidate/failure");
            let value1 = ArtifactValue::file(digest_config.empty_file());
            let value2 = ArtifactValue::dir(digest_config.empty_directory());

            // Start from having something.
            dm.testing_declare_existing(&path, value1);

            // This will collect the existing future and invalidate, and then fail in doing so.
            dm.testing_declare(&path, value2);

            // Now we check that materialization fails. This needs to wait on the previous clean.
            let res = dm
                .materialize_artifact(&path, EventDispatcher::null())
                .ok_or_else(|| internal_error!("Expected a future"))?
                .await;

            assert_matches!(
            res,
            Err(SharedMaterializingError::Error(e)) if format!("{e:#}").contains("Injected error")
        );

            // We do not actually get to materializing or cleaning.
            assert_eq!(dm.io.take_log(), &[]);

            Ok(())
        }).await
    }

    #[tokio::test]
    async fn test_materialize_dep_error() -> buck2_error::Result<()> {
        ignore_stack_overflow_checks_for_future(async {
            // Construct a tree with a symlink and its target, materialize both at once
            let symlink_path = make_path("foo/bar_symlink");
            let target_path = make_path("foo/bar_target");
            let target_from_symlink = RelativePathBuf::from_path(Path::new("bar_target"))?;

            let (mut dm, mut channel) = make_processor(Default::default());
            let digest_config = dm.io.digest_config();

            let target_value = ArtifactValue::file(digest_config.empty_file());
            let symlink_value = make_artifact_value_with_symlink_dep(
                &target_path,
                &target_from_symlink,
                digest_config,
            )?;
            // Declare and materialize symlink and target
            dm.testing_declare(
                &target_path,
                target_value.clone(),
            );
            dm.testing_declare(
                &symlink_path,
                symlink_value.clone(),
            );
            dm.materialize_artifact(&symlink_path, EventDispatcher::null())
                .ok_or_else(|| internal_error!("Expected a future"))?
                .await
                .map_err(|err| buck2_error!(buck2_error::ErrorTag::MaterializationError, "error materializing {:?}", err))?;
            assert_eq!(
                dm.io.take_log(),
                &[
                    (Op::Clean, target_path.clone()),
                    (Op::Clean, symlink_path.clone()),
                    (Op::Materialize, target_path.clone()),
                    (Op::Materialize, symlink_path.clone()),
                ]
            );

            // Process materialization_finished, change symlink stage to materialized
            while let Ok(cmd) = channel.low_priority.try_recv() {
                dm.testing_process_one_low_priority_command(cmd);
            }

            // Change symlink target value and re-declare
            let content = b"not empty";
            let meta = FileMetadata {
                digest: TrackedFileDigest::from_content(content, digest_config.cas_digest_config()),
                is_executable: false,
            };
            let target_value = ArtifactValue::file(meta);
            dm.testing_declare(
                &target_path,
                target_value,
            );
            assert_eq!(dm.io.take_log(), &[(Op::Clean, target_path.clone())]);

            // Request to materialize symlink, fail to materialize target
            dm.io.set_fail_on(vec![target_path.clone()]);
            let res = dm
                .materialize_artifact(&symlink_path, EventDispatcher::null())
                .ok_or_else(|| internal_error!("Expected a future"))?
                .await;
            assert_matches!(
            res,
            Err(SharedMaterializingError::Error(e)) if format!("{e:#}").contains("Injected error")
        );
            assert_eq!(
                dm.io.take_log(),
                &[(Op::MaterializeError, target_path.clone())]
            );
            // Process materialization_finished, _only_ target is cleaned, not symlink
            while let Ok(cmd) = channel.low_priority.try_recv() {
                dm.testing_process_one_low_priority_command(cmd);
            }
            assert_eq!(dm.io.take_log(), &[(Op::Clean, target_path.clone())]);

            // Request symlink again, target is materialized and symlink materialization succeeds
            dm.io.set_fail_on(vec![]);
            dm.materialize_artifact(&symlink_path, EventDispatcher::null())
                .ok_or_else(|| internal_error!("Expected a future"))?
                .await
                .map_err(|err| buck2_error!(buck2_error::ErrorTag::MaterializationError, "error materializing 2 {:?}", err))?;
            assert_eq!(dm.io.take_log(), &[(Op::Materialize, target_path.clone()), ]);
            Ok(())
        }).await
    }

    #[tokio::test]
    async fn test_retry() -> buck2_error::Result<()> {
        ignore_stack_overflow_checks_for_future(async {
            let (mut dm, mut channel) = make_processor(Default::default());
            let digest_config = dm.io.digest_config();

            let path = make_path("test");
            let value1 = ArtifactValue::file(digest_config.empty_file());

            // Declare a value.
            dm.testing_declare(&path, value1);

            // Make materializations fail
            dm.io.set_fail(true);

            // Materializing it fails.
            let res = dm
                .materialize_artifact(&path, EventDispatcher::null())
                .ok_or_else(|| internal_error!("Expected a future"))?
                .await;

            assert_matches!(
                res,
                Err(SharedMaterializingError::Error(e)) if format!("{e:#}").contains("Injected error")
            );

            // Unset fail, but we haven't processed materialization_finished yet so this does nothing.
            dm.io.set_fail(false);

            // Rejoining the existing future fails.
            let res = dm
                .materialize_artifact(&path, EventDispatcher::null())
                .ok_or_else(|| internal_error!("Expected a future"))?
                .await;

            assert_matches!(
                res,
                Err(SharedMaterializingError::Error(e)) if format!("{e:#}").contains("Injected error")
            );

            // Now process cleanup_finished_vacant and materialization_finished.
            let mut processed = 0;

            while let Ok(cmd) = channel.low_priority.try_recv() {
                eprintln!("got cmd = {cmd:?}");
                dm.testing_process_one_low_priority_command(cmd);
                processed += 1;
            }

            assert_eq!(processed, 2);

            // Materializing works now:
            let res = dm
                .materialize_artifact(&path, EventDispatcher::null())
                .ok_or_else(|| internal_error!("Expected a future"))?
                .await;

            assert_matches!(res, Ok(()));

            Ok(())
        }).await
    }

    const SAMPLE_BUCK_OUT_PATH: &str = "buck-out/v2/gen/foo/bar";

    #[tokio::test]
    async fn test_clean_stale() -> buck2_error::Result<()> {
        ignore_stack_overflow_checks_for_future(async {
            let path = make_path(SAMPLE_BUCK_OUT_PATH);
            let project_root = temp_root();
            let io = Arc::new(StubIoHandler::new(project_root.clone()));
            let (dm, mut handle, _) = make_materializer(io.dupe(), None).await;
            materialize_write(&path, b"contents", &mut handle, &dm).await?;
            // Drop dm and flush sqlite connection.
            dm.abort();
            // Create new materializer from db state so that artifacts are not active
            let (dm, _, _) = make_materializer(io, None).await;

            let res = dm
                .clean_stale_artifacts(DateTime::<Utc>::MAX_UTC, false, false)
                .await?;

            let &buck2_data::CleanStaleStats {
                stale_artifact_count,
                stale_bytes,
                cleaned_artifact_count,
                cleaned_bytes,
                ..
            } = res
                .stats
                .as_ref()
                .unwrap_or_else(|| panic!("{}", res.message.unwrap()));
            assert_eq!(
                (
                    stale_artifact_count,
                    stale_bytes,
                    cleaned_artifact_count,
                    cleaned_bytes
                ),
                (1, 8, 1, 8)
            );
            Ok(())
        })
        .await
    }

    #[tokio::test]
    async fn test_clean_stale_interrupt() -> buck2_error::Result<()> {
        ignore_stack_overflow_checks_for_future(async {
            let path = make_path(SAMPLE_BUCK_OUT_PATH);
            let project_root = temp_root();
            let io = Arc::new(StubIoHandler::new(project_root.clone()));
            let (dm, mut handle, _) = make_materializer(io.dupe(), None).await;
            materialize_write(&path, b"contents", &mut handle, &dm).await?;

            let read_dir_barriers =
                Arc::new((std::sync::Barrier::new(2), std::sync::Barrier::new(2)));
            let io = Arc::new(
                StubIoHandler::new(project_root.dupe())
                    .with_read_dir_barriers(read_dir_barriers.dupe()),
            );
            let (dm, _, _) = make_materializer(io, None).await;

            // Interrupt while scanning buck-out
            let dm = Arc::new(dm);
            let dm_dup = dm.dupe();
            let fut = dm_dup.clean_stale_artifacts(DateTime::<Utc>::MAX_UTC, false, false);
            thread::spawn(move || {
                // Wait until a read_dir request is about to execute
                read_dir_barriers.0.wait();
                // Sending a high_priority command will interrupt the processor
                let noop_command = MaterializerCommand::DeclareExisting(vec![], None, None);
                let _unused = dm.command_sender.send(noop_command);
                // Wait after sending so that a second request doesn't start
                read_dir_barriers.1.wait();
            });
            let res = fut.await?;
            let &buck2_data::CleanStaleStats {
                stale_artifact_count,
                stale_bytes,
                cleaned_artifact_count,
                cleaned_bytes,
                ..
            } = res.stats.as_ref().unwrap();
            assert_eq!(
                (
                    stale_artifact_count,
                    stale_bytes,
                    cleaned_artifact_count,
                    cleaned_bytes
                ),
                (0, 0, 0, 0)
            );

            let clean_barriers = Arc::new((Barrier::new(2), Barrier::new(2)));
            let io = Arc::new(
                StubIoHandler::new(project_root.dupe()).with_clean_barriers(clean_barriers.dupe()),
            );
            let (dm, _, _) = make_materializer(io, None).await;

            // Interrupt while deleting files
            let dm = Arc::new(dm);
            let dm_dup = dm.dupe();
            let fut = dm_dup.clean_stale_artifacts(DateTime::<Utc>::MAX_UTC, false, false);
            thread::spawn(move || {
                // Wait until a single clean request is about to execute
                clean_barriers.0.wait();
                // Sending a high_priority command will drop the clean guard immediately (from this thread)
                let noop_command = MaterializerCommand::DeclareExisting(vec![], None, None);
                let _unused = dm.command_sender.send(noop_command);
                // Wait after sending, executing clean request will complete but a second request doesn't start because
                // the single io thread is blocked
                clean_barriers.1.wait();
            });
            let res = fut.await?;
            let &buck2_data::CleanStaleStats {
                stale_artifact_count,
                stale_bytes,
                cleaned_artifact_count,
                cleaned_bytes,
                ..
            } = res.stats.as_ref().unwrap();
            assert_eq!(
                (
                    stale_artifact_count,
                    stale_bytes,
                    cleaned_artifact_count,
                    cleaned_bytes
                ),
                (1, 8, 0, 0)
            );

            Ok(())
        })
        .await
    }

    #[tokio::test]
    async fn test_clean_stale_schedule() -> buck2_error::Result<()> {
        ignore_stack_overflow_checks_for_future(async {
            let path = make_path(SAMPLE_BUCK_OUT_PATH);
            let project_root = temp_root();
            // dry run because it's easier and since this is only testing that cleans are triggered by the materializer
            let clean_stale_config = CleanStaleConfig {
                clean_period: std::time::Duration::from_secs(1),
                artifact_ttl: std::time::Duration::from_secs(0),
                start_offset: std::time::Duration::from_secs(0),
                decreased_ttl_hours_disk_threshold: None,
                decreased_ttl_hours: None,
                dry_run: true,
            };
            let io = Arc::new(StubIoHandler::new(project_root.dupe()));
            let (dm, mut handle, mut daemon_dispatcher_events) =
                make_materializer(io.dupe(), Some(clean_stale_config)).await;
            materialize_write(&path, b"contents", &mut handle, &dm).await?;

            let receive_clean_result = |events: &mut ChannelEventSource| {
                let event = events.receive().unwrap();
                match event.unpack_buck().unwrap().data() {
                    buck2_data::buck_event::Data::Instant(instant) => match instant.data.as_ref() {
                        Some(buck2_data::instant_event::Data::CleanStaleResult(res)) => {
                            Some(res.clone())
                        }
                        _ => None,
                    },
                    _ => None,
                }
                .unwrap()
            };
            // The first clean stale request is scheduled at roughly the same time as materialize_write so we may receive an initial clean event
            // before anything is materialized, if so ignore events until an artifact is found (retained != 0).
            // It should only be necessary to wait for a single clean (1 second) but wait for up to 5 just in case.
            let mut i = 0;
            while i < 5 {
                let res = receive_clean_result(&mut daemon_dispatcher_events);
                let stats = res.stats.unwrap();
                if let buck2_data::CleanStaleStats {
                    retained_artifact_count: 0,
                    ..
                } = stats
                {
                    i += 1;
                } else {
                    break;
                }
            }
            let res = receive_clean_result(&mut daemon_dispatcher_events);
            let buck2_data::CleanStaleStats {
                retained_artifact_count,
                ..
            } = res.stats.unwrap();
            assert_eq!(retained_artifact_count, 1);
            // check it's scheduled more than once
            let res = receive_clean_result(&mut daemon_dispatcher_events);
            let buck2_data::CleanStaleStats {
                retained_artifact_count,
                ..
            } = res.stats.unwrap();
            assert_eq!(retained_artifact_count, 1);
            Ok(())
        })
        .await
    }

    #[tokio::test]
    async fn test_has_artifact_at() -> buck2_error::Result<()> {
        ignore_stack_overflow_checks_for_future(async {
            let (mut dm, _) = make_processor(Default::default());
            let digest_config = dm.io.digest_config();

            let path = make_path("test/dir/path");
            let value1 = ArtifactValue::dir(digest_config.empty_directory());
            dm.testing_declare_existing(&path, value1);

            assert!(dm.testing_has_artifact(path.clone()));
            assert!(!dm.testing_has_artifact(path.join(ForwardRelativePath::new("foo").unwrap())));
            assert!(!dm.testing_has_artifact(path.parent().unwrap().to_owned()));

            dm.materialize_artifact(&path, EventDispatcher::null());
            assert!(dm.testing_has_artifact(path.clone()));
            assert!(!dm.testing_has_artifact(path.join(ForwardRelativePath::new("foo").unwrap())));
            assert!(!dm.testing_has_artifact(path.parent().unwrap().to_owned()));

            Ok(())
        })
        .await
    }
}
