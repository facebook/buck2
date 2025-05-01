/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_execute::directory::ActionDirectory;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionDirectoryRef;

use crate::materializers::deferred::data_tree::DataTree;

pub type FileTree<V> = DataTree<FileNameBuf, V>;

struct NoopCollector;

impl<'a> FromIterator<&'a FileNameBuf> for NoopCollector {
    fn from_iter<I>(_iter: I) -> Self
    where
        I: IntoIterator<Item = &'a FileNameBuf>,
    {
        NoopCollector
    }
}

impl<V: 'static> FileTree<V> {
    pub fn iter_with_paths(&self) -> impl Iterator<Item = (ForwardRelativePathBuf, &V)> {
        self.iter::<ForwardRelativePathBuf>()
    }

    pub fn iter_without_paths(&self) -> impl Iterator<Item = &V> {
        self.iter::<NoopCollector>().map(|(NoopCollector, v)| v)
    }

    pub fn into_iter_with_paths(self) -> impl Iterator<Item = (ForwardRelativePathBuf, V)> {
        self.into_iter::<ForwardRelativePathBuf>()
    }

    #[allow(unused)]
    pub fn into_iter_without_paths(self) -> impl Iterator<Item = V> {
        self.into_iter::<NoopCollector>()
            .map(|(NoopCollector, v)| v)
    }
}

enum FoundArtifact {
    /// Proper artifact.
    Found,
    /// Found a directory artifact with dependencies inside it.
    FoundForDir,
    // TODO(nga): figure the meaning of remaining. Are these bugs?
    /// Dependency dir not found in tree.
    DirNotFound,
    /// Leaf pointing to a dir.
    LeafPointsToDir,
}

impl<V: 'static> FileTree<V> {
    /// Finds all the paths in `deps` that are artifacts in `self`
    pub fn find_artifacts<D>(&self, deps: &D) -> Vec<ProjectRelativePathBuf>
    where
        D: ActionDirectory,
    {
        let mut artifacts = Vec::new();
        self.find_artifacts_impl(deps, |path, found| match found {
            FoundArtifact::Found | FoundArtifact::FoundForDir => {
                artifacts.push(path.to_buf());
            }
            FoundArtifact::DirNotFound | FoundArtifact::LeafPointsToDir => {}
        });
        artifacts
    }

    pub fn find_artifacts_for_debug<D>(
        &self,
        deps: &D,
    ) -> Vec<(ProjectRelativePathBuf, &'static str)>
    where
        D: ActionDirectory,
    {
        let mut result = Vec::new();
        self.find_artifacts_impl(deps, |path, found| {
            let found = match found {
                FoundArtifact::Found => "Found",
                FoundArtifact::FoundForDir => "FoundForDir",
                FoundArtifact::DirNotFound => "DirNotFound",
                FoundArtifact::LeafPointsToDir => "LeafPointsToDir",
            };
            result.push((path.to_buf(), found));
        });
        result
    }

    fn find_artifacts_impl<D>(
        &self,
        deps: &D,
        mut listener: impl FnMut(&ProjectRelativePath, FoundArtifact),
    ) where
        D: ActionDirectory,
    {
        fn walk_deps<'a, V, D>(
            tree: &FileTree<V>,
            entry: DirectoryEntry<D, &ActionDirectoryMember>,
            path: &mut ProjectRelativePathBuf,
            listener: &mut impl FnMut(&ProjectRelativePath, FoundArtifact),
        ) where
            D: ActionDirectoryRef<'a>,
        {
            match (tree, entry) {
                (FileTree::Data(_), DirectoryEntry::Leaf(_)) => {
                    listener(path, FoundArtifact::Found);
                }
                (FileTree::Data(_), DirectoryEntry::Dir(_)) => {
                    listener(path, FoundArtifact::FoundForDir);
                }
                (FileTree::Tree(tree_children), DirectoryEntry::Dir(d)) => {
                    // Not an artifact, but if entry is a directory we can search deeper within
                    for (name, child) in d.entries() {
                        path.push(name);
                        if let Some(subtree) = tree_children.get(name) {
                            walk_deps(subtree, child, path, listener);
                        } else {
                            listener(path, FoundArtifact::DirNotFound);
                        }
                        let popped = path.pop();
                        assert!(popped);
                    }
                }
                (FileTree::Tree(_), DirectoryEntry::Leaf(_)) => {
                    listener(path, FoundArtifact::LeafPointsToDir);
                }
            }
        }

        let mut path_buf = ProjectRelativePathBuf::default();
        walk_deps(
            self,
            DirectoryEntry::Dir(Directory::as_ref(deps)),
            &mut path_buf,
            &mut listener,
        );
        assert!(path_buf.is_empty());
    }

    /// Removes path from FileTree. Returns an iterator of pairs of path and entry removed
    /// from the tree.
    pub fn remove_path(
        &mut self,
        path: &ProjectRelativePath,
    ) -> Box<dyn Iterator<Item = (ProjectRelativePathBuf, V)>> {
        let mut path_iter = path.iter();
        let removed = self.remove(&mut path_iter);

        let mut path = path;
        // Rewind the `path` up to the entry we *actually* found.
        for _ in path_iter {
            path = path
                .parent()
                .expect("Path iterator cannot cause us to rewind past the last parent");
        }
        let path = path.to_owned();

        match removed {
            Some(tree) => Box::new(
                tree.into_iter_with_paths()
                    .map(move |(k, v)| ((path).join(k), v)),
            ),
            None => Box::new(std::iter::empty()),
        }
    }
}
