/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;

use buck2_common::file_ops::FileMetadata;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::insert_file;
use buck2_execute::directory::ActionDirectoryBuilder;
use dupe::Dupe;

use super::Version;
use super::VersionTracker;
use super::*;

#[test]
fn test_find_artifacts() -> anyhow::Result<()> {
    let artifact1 = ProjectRelativePathBuf::unchecked_new("foo/bar/baz".to_owned());
    let artifact2 = ProjectRelativePathBuf::unchecked_new("foo/bar/bar/qux".to_owned());
    let artifact3 = ProjectRelativePathBuf::unchecked_new("foo/bar/bar/quux".to_owned());
    let artifact4 = ProjectRelativePathBuf::unchecked_new("foo/bar/qux/quuz".to_owned());
    let non_artifact1 = ProjectRelativePathBuf::unchecked_new("foo/bar/qux".to_owned());
    let non_artifact2 = ProjectRelativePathBuf::unchecked_new("foo/bar/bar/corge".to_owned());

    let file = FileMetadata::empty(DigestConfig::compat().cas_digest_config());

    // Build deps with artifacts 1-3, and non-artifacts 1-2
    let mut builder = ActionDirectoryBuilder::empty();
    insert_file(
        &mut builder,
        artifact1.join_normalized("f1")?.as_ref(),
        file.dupe(),
    )?;
    insert_file(
        &mut builder,
        artifact2.join_normalized("d/f1")?.as_ref(),
        file.dupe(),
    )?;
    insert_file(&mut builder, artifact3.as_ref(), file.dupe())?;
    insert_file(&mut builder, non_artifact2.as_ref(), file.dupe())?;
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

mod state_machine {
    use std::path::Path;

    use buck2_execute::directory::Symlink;
    use buck2_execute::directory::INTERNER;
    use once_cell::sync::Lazy;
    use parking_lot::Mutex;
    use tokio::time::sleep;
    use tokio::time::Duration as TokioDuration;

    use super::*;

    #[derive(Debug, Eq, PartialEq)]
    enum Op {
        Clean,
        Materialize,
    }

    #[derive(Default)]
    struct StubIoHandler {
        log: Mutex<Vec<(Op, ProjectRelativePathBuf)>>,
        // If set, add a sleep when materializing to simulate a long materialization period
        materialization_config: HashMap<ProjectRelativePathBuf, TokioDuration>,
    }

    impl StubIoHandler {
        fn take_log(&self) -> Vec<(Op, ProjectRelativePathBuf)> {
            std::mem::take(&mut *self.log.lock())
        }

        pub fn new(materialization_config: HashMap<ProjectRelativePathBuf, TokioDuration>) -> Self {
            Self {
                log: Default::default(),
                materialization_config,
            }
        }
    }

    #[async_trait]
    impl IoHandler for StubIoHandler {
        fn write(
            self: &Arc<Self>,
            path: ProjectRelativePathBuf,
            _write: Arc<WriteFile>,
            _version: Version,
            _command_sender: MaterializerSender<Self>,
        ) -> BoxFuture<'static, Result<(), SharedMaterializingError>> {
            self.log.lock().push((Op::Materialize, path));
            futures::future::ready(Ok(())).boxed()
        }

        fn clean_output_paths(
            self: &Arc<Self>,
            paths: Vec<ProjectRelativePathBuf>,
        ) -> BoxFuture<'static, Result<(), SharedError>> {
            for path in paths {
                self.log.lock().push((Op::Clean, path));
            }
            futures::future::ready(Ok(())).boxed()
        }

        async fn materialize_entry(
            self: &Arc<Self>,
            path: ProjectRelativePathBuf,
            _method: Arc<ArtifactMaterializationMethod>,
            _entry: ActionDirectoryEntry<ActionSharedDirectory>,
            _event_dispatcher: EventDispatcher,
        ) -> Result<(), MaterializeEntryError> {
            // Simulate a non-immediate materialization if configured
            match self.materialization_config.get(&path) {
                Some(duration) => {
                    sleep(*duration).await;
                }
                None => (),
            }
            self.log.lock().push((Op::Materialize, path));
            Ok(())
        }

        fn create_ttl_refresh(
            self: &Arc<Self>,
            _tree: &ArtifactTree,
            _min_ttl: Duration,
        ) -> Option<BoxFuture<'static, anyhow::Result<()>>> {
            None
        }
    }

    /// A stub command sender. We are calling materializer methods directly so that's all we need.
    fn command_sender() -> MaterializerSender<StubIoHandler> {
        static SENDER: Lazy<MaterializerSender<StubIoHandler>> = Lazy::new(|| MaterializerSender {
            high_priority: Box::leak(box mpsc::unbounded_channel().0),
            low_priority: Box::leak(box mpsc::unbounded_channel().0),
            counters: MaterializerCounters::leak_new(),
        });

        *SENDER
    }

    fn make_path(p: &str) -> ProjectRelativePathBuf {
        ProjectRelativePath::new(p).unwrap().to_owned()
    }

    #[tokio::test]
    async fn test_declare_reuse() -> anyhow::Result<()> {
        let digest_config = DigestConfig::compat();

        let mut dm = DeferredMaterializerCommandProcessor {
            io: Arc::new(StubIoHandler::default()),
            digest_config,
            sqlite_db: None,
            rt: Handle::current(),
            defer_write_actions: true,
            log_buffer: LogBuffer::new(1),
        };

        let mut tree = ArtifactTree::new();
        let path = make_path("foo/bar");
        let value = ArtifactValue::file(digest_config.empty_file());
        let method = ArtifactMaterializationMethod::Test;
        let mut version_tracker = VersionTracker::new();

        dm.declare(
            &mut tree,
            path.clone(),
            value,
            box method,
            &mut version_tracker,
            &command_sender(),
        );
        assert_eq!(dm.io.take_log(), &[(Op::Clean, path.clone())]);

        let res = dm
            .materialize_artifact(&mut tree, &path, EventDispatcher::null(), &command_sender())
            .context("Expected a future")?
            .await;
        assert_eq!(dm.io.take_log(), &[(Op::Materialize, path.clone())]);

        // This API is a bit odd -_-
        tree.materialization_finished(
            path.clone(),
            Utc::now(),
            version_tracker.current(),
            res,
            &dm.io,
            &mut version_tracker,
            dm.sqlite_db.as_mut(),
            &dm.rt,
        );
        assert_eq!(dm.io.take_log(), &[]);

        Ok(())
    }

    fn make_artifact_value_with_symlink_dep(
        target_path: &ProjectRelativePathBuf,
        target_from_symlink: &RelativePathBuf,
        digest_config: DigestConfig,
    ) -> anyhow::Result<ArtifactValue> {
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
    async fn test_materialize_symlink_and_target() -> anyhow::Result<()> {
        let digest_config = DigestConfig::compat();

        // Construct a tree with a symlink and its target, materialize both at once
        let mut tree = ArtifactTree::new();
        let symlink_path = make_path("foo/bar_symlink");
        let target_path = make_path("foo/bar_target");
        let target_from_symlink = RelativePathBuf::from_path(Path::new("bar_target"))?;

        let mut materialization_config = HashMap::new();
        // Materialize the symlink target slowly so that we actually hit the logic point where we
        // await for symlink targets and the entry materialization
        materialization_config.insert(target_path.clone(), TokioDuration::from_millis(100));

        let mut version_tracker = VersionTracker::new();

        let mut dm = DeferredMaterializerCommandProcessor {
            io: Arc::new(StubIoHandler::new(materialization_config)),
            digest_config,
            sqlite_db: None,
            rt: Handle::current(),
            defer_write_actions: true,
            log_buffer: LogBuffer::new(1),
        };

        // Declare symlink target
        dm.declare(
            &mut tree,
            target_path.clone(),
            ArtifactValue::file(digest_config.empty_file()),
            box ArtifactMaterializationMethod::Test,
            &mut version_tracker,
            &command_sender(),
        );
        assert_eq!(dm.io.take_log(), &[(Op::Clean, target_path.clone())]);

        // Declare symlink
        let symlink_value = make_artifact_value_with_symlink_dep(
            &target_path,
            &target_from_symlink,
            digest_config,
        )?;
        dm.declare(
            &mut tree,
            symlink_path.clone(),
            symlink_value,
            box ArtifactMaterializationMethod::Test,
            &mut version_tracker,
            &command_sender(),
        );
        assert_eq!(dm.io.take_log(), &[(Op::Clean, symlink_path.clone())]);

        dm.materialize_artifact(
            &mut tree,
            &symlink_path,
            EventDispatcher::null(),
            &command_sender(),
        )
        .context("Expected a future")?
        .await
        .map_err(|_| anyhow::anyhow!("error materializing"))?;

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
    }

    #[tokio::test]
    async fn test_materialize_symlink_first_then_target() -> anyhow::Result<()> {
        let digest_config = DigestConfig::compat();

        // Materialize a symlink, then materialize the target. Test that we still
        // materialize deps if the main artifact has already been materialized.
        let mut tree = ArtifactTree::new();
        let symlink_path = make_path("foo/bar_symlink");
        let target_path = make_path("foo/bar_target");
        let target_from_symlink = RelativePathBuf::from_path(Path::new("bar_target"))?;

        let mut materialization_config = HashMap::new();
        // Materialize the symlink target slowly so that we actually hit the logic point where we
        // await for symlink targets and the entry materialization
        materialization_config.insert(target_path.clone(), TokioDuration::from_millis(100));

        let mut version_tracker = VersionTracker::new();

        let mut dm = DeferredMaterializerCommandProcessor {
            io: Arc::new(StubIoHandler::new(materialization_config)),
            sqlite_db: None,
            rt: Handle::current(),
            defer_write_actions: true,
            log_buffer: LogBuffer::new(1),
            digest_config,
        };

        // Declare symlink
        let symlink_value = make_artifact_value_with_symlink_dep(
            &target_path,
            &target_from_symlink,
            digest_config,
        )?;
        dm.declare(
            &mut tree,
            symlink_path.clone(),
            symlink_value,
            box ArtifactMaterializationMethod::Test,
            &mut version_tracker,
            &command_sender(),
        );
        assert_eq!(dm.io.take_log(), &[(Op::Clean, symlink_path.clone())]);

        // Materialize the symlink, at this point the target is not in the tree so it's ignored
        let res = dm
            .materialize_artifact(
                &mut tree,
                &symlink_path,
                EventDispatcher::null(),
                &command_sender(),
            )
            .context("Expected a future")?
            .await;

        let logs = dm.io.take_log();
        assert_eq!(logs, &[(Op::Materialize, symlink_path.clone())]);

        // Mark the symlink as materialized
        tree.materialization_finished(
            symlink_path.clone(),
            Utc::now(),
            version_tracker.current(),
            res,
            &dm.io,
            &mut version_tracker,
            dm.sqlite_db.as_mut(),
            &dm.rt,
        );
        assert_eq!(dm.io.take_log(), &[]);

        // Declare symlink target
        dm.declare(
            &mut tree,
            target_path.clone(),
            ArtifactValue::file(digest_config.empty_file()),
            box ArtifactMaterializationMethod::Test,
            &mut version_tracker,
            &command_sender(),
        );
        assert_eq!(dm.io.take_log(), &[(Op::Clean, target_path.clone())]);

        // Materialize the symlink again.
        // This time, we don't re-materialize the symlink as that's already been done.
        // But we still materialize the target as that has not been materialized yet.
        dm.materialize_artifact(
            &mut tree,
            &symlink_path,
            EventDispatcher::null(),
            &command_sender(),
        )
        .context("Expected a future")?
        .await
        .map_err(|_| anyhow::anyhow!("error materializing"))?;

        let logs = dm.io.take_log();
        assert_eq!(logs, &[(Op::Materialize, target_path.clone())]);

        Ok(())
    }
}
