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
use buck2_execute::directory::insert_file;
use buck2_execute::directory::ActionDirectoryBuilder;
use gazebo::dupe::Dupe;

use super::*;

#[test]
fn test_find_artifacts() -> anyhow::Result<()> {
    let artifact1 = ProjectRelativePathBuf::unchecked_new("foo/bar/baz".to_owned());
    let artifact2 = ProjectRelativePathBuf::unchecked_new("foo/bar/bar/qux".to_owned());
    let artifact3 = ProjectRelativePathBuf::unchecked_new("foo/bar/bar/quux".to_owned());
    let artifact4 = ProjectRelativePathBuf::unchecked_new("foo/bar/qux/quuz".to_owned());
    let non_artifact1 = ProjectRelativePathBuf::unchecked_new("foo/bar/qux".to_owned());
    let non_artifact2 = ProjectRelativePathBuf::unchecked_new("foo/bar/bar/corge".to_owned());

    let file = FileMetadata::empty();

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
    use once_cell::sync::Lazy;
    use parking_lot::Mutex;

    use super::*;

    #[derive(Debug, Eq, PartialEq)]
    enum Op {
        Clean,
        Materialize,
    }

    #[derive(Default)]
    struct StubIoHandler {
        log: Mutex<Vec<(Op, ProjectRelativePathBuf)>>,
    }

    impl StubIoHandler {
        fn take_log(&self) -> Vec<(Op, ProjectRelativePathBuf)> {
            std::mem::take(&mut *self.log.lock())
        }
    }

    #[async_trait]
    impl IoHandler for StubIoHandler {
        fn write(
            self: &Arc<Self>,
            path: ProjectRelativePathBuf,
            _write: Arc<WriteFile>,
            _version: u64,
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
        static SENDER: Lazy<MaterializerSender<StubIoHandler>> = Lazy::new(|| {
            let (tx, _rx) = mpsc::unbounded_channel();
            MaterializerSender {
                sender: Box::leak(box tx),
                counters: MaterializerCounters::leak_new(),
            }
        });

        *SENDER
    }

    fn make_path(p: &str) -> ProjectRelativePathBuf {
        ProjectRelativePath::new(p).unwrap().to_owned()
    }

    #[tokio::test]
    async fn test_declare_reuse() -> anyhow::Result<()> {
        let mut dm = DeferredMaterializerCommandProcessor {
            io: Arc::new(StubIoHandler::default()),
            sqlite_db: None,
            rt: Handle::current(),
        };

        let mut tree = ArtifactTree::new();
        let path = make_path("foo/bar");
        let value = ArtifactValue::empty_file();
        let method = ArtifactMaterializationMethod::Test;

        dm.declare(
            &mut tree,
            path.clone(),
            value,
            box method,
            0,
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
            0,
            res,
            &dm.io,
            1,
            dm.sqlite_db.as_mut(),
            &dm.rt,
        );
        assert_eq!(dm.io.take_log(), &[]);

        Ok(())
    }
}
