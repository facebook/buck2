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
use std::fmt;
use std::fmt::Debug;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context as _;
use buck2_common::cas_digest::CasDigestConfig;
use buck2_common::cas_digest::DigestAlgorithm;
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_common::external_symlink::ExternalSymlink;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::directory::find;
use buck2_core::directory::unordered_entry_walk;
use buck2_core::directory::DashMapDirectoryInterner;
use buck2_core::directory::Directory;
use buck2_core::directory::DirectoryBuilder;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::DirectoryHasher;
use buck2_core::directory::DirectoryIterator;
use buck2_core::directory::DirectorySelector;
use buck2_core::directory::FingerprintedDirectory;
use buck2_core::directory::ImmutableDirectory;
use buck2_core::directory::SharedDirectory;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::RelativePath;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use chrono::DateTime;
use chrono::Utc;
use derive_more::Display;
use dupe::Dupe;
use once_cell::sync::Lazy;
use ref_cast::RefCast;
use remote_execution as RE;
use starlark_map::small_map::SmallMap;
use thiserror::Error;

use crate::artifact_value::ArtifactValue;
use crate::digest::CasDigestFromReExt;
use crate::digest::CasDigestToReExt;
use crate::digest_config::DigestConfig;
use crate::re::manager::ManagedRemoteExecutionClient;

#[allocative::root]
pub static INTERNER: Lazy<DashMapDirectoryInterner<ActionDirectoryMember, TrackedFileDigest>> =
    Lazy::new(DashMapDirectoryInterner::new);

/// Represents a relative symlink, and stores the symlink's target path.
#[derive(Debug, Display, Eq, PartialEq, Allocative)]
pub struct Symlink(RelativePathBuf);

#[derive(Clone, Debug, Dupe, PartialEq, Eq, Display, Allocative)]
pub enum ActionDirectoryMember {
    File(FileMetadata),
    Symlink(Arc<Symlink>),
    ExternalSymlink(Arc<ExternalSymlink>),
}

pub type ActionDirectoryEntry<D> = DirectoryEntry<D, ActionDirectoryMember>;

pub type ActionImmutableDirectory = ImmutableDirectory<ActionDirectoryMember, TrackedFileDigest>;

pub type ActionSharedDirectory = SharedDirectory<ActionDirectoryMember, TrackedFileDigest>;

pub type ActionDirectoryBuilder = DirectoryBuilder<ActionDirectoryMember, TrackedFileDigest>;

pub trait ActionDirectory = Directory<ActionDirectoryMember, TrackedFileDigest>;

pub trait ActionFingerprintedDirectory =
    FingerprintedDirectory<ActionDirectoryMember, TrackedFileDigest>;

#[derive(Allocative, RefCast)]
#[repr(transparent)]
pub struct ReDirectorySerializer {
    pub cas_digest_config: CasDigestConfig,
}

impl ReDirectorySerializer {
    fn create_re_directory<'a, D, I>(entries: I) -> RE::Directory
    where
        I: IntoIterator<
            Item = (
                &'a FileName,
                DirectoryEntry<&'a D, &'a ActionDirectoryMember>,
            ),
        >,
        D: ActionFingerprintedDirectory + ?Sized + 'a,
    {
        let mut files: Vec<RE::FileNode> = Vec::new();
        let mut directories: Vec<RE::DirectoryNode> = Vec::new();
        let mut symlinks: Vec<RE::SymlinkNode> = Vec::new();

        for (name, entry) in entries {
            let name = name.as_str().into();

            match entry {
                DirectoryEntry::Dir(d) => {
                    directories.push(RE::DirectoryNode {
                        name,
                        digest: Some(d.fingerprint().to_grpc()),
                    });
                }
                DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => {
                    files.push(RE::FileNode {
                        name,
                        digest: Some(f.digest.to_grpc()),
                        is_executable: f.is_executable,
                        ..Default::default()
                    });
                }
                DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(s)) => {
                    symlinks.push(RE::SymlinkNode {
                        name,
                        target: s.to_string(),
                        ..Default::default()
                    });
                }
                DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(s)) => {
                    symlinks.push(RE::SymlinkNode {
                        name,
                        target: s.target_str().to_owned(),
                        ..Default::default()
                    });
                }
            }
        }

        files.sort_by(|a, b| a.name.cmp(&b.name));
        directories.sort_by(|a, b| a.name.cmp(&b.name));
        symlinks.sort_by(|a, b| a.name.cmp(&b.name));

        RE::Directory {
            files,
            directories,
            symlinks,
            ..Default::default()
        }
    }

    pub fn serialize_entries<'a, D, I>(entries: I) -> Vec<u8>
    where
        I: IntoIterator<
            Item = (
                &'a FileName,
                DirectoryEntry<&'a D, &'a ActionDirectoryMember>,
            ),
        >,
        D: ActionFingerprintedDirectory + ?Sized + 'a,
    {
        proto_serialize(&Self::create_re_directory(entries))
    }
}

fn proto_serialize<M: prost::Message>(m: &M) -> Vec<u8> {
    let mut serialized_buf = Vec::new();
    m.encode(&mut serialized_buf).unwrap();
    serialized_buf
}

impl DirectoryHasher<ActionDirectoryMember, TrackedFileDigest> for ReDirectorySerializer {
    fn hash_entries<'a, D, I>(&self, entries: I) -> TrackedFileDigest
    where
        I: IntoIterator<
            Item = (
                &'a FileName,
                DirectoryEntry<&'a D, &'a ActionDirectoryMember>,
            ),
        >,
        D: ActionFingerprintedDirectory + 'a,
    {
        TrackedFileDigest::from_content(&Self::serialize_entries(entries), self.cas_digest_config)
    }
}

impl Symlink {
    pub fn new(target: RelativePathBuf) -> Self {
        Self(target)
    }

    /// Returns the path the symlink points to.
    pub fn target(&self) -> &RelativePath {
        self.0.as_relative_path()
    }

    /// Creates a new `Symlink` from `self`, such that its target is
    /// `src_relative_to_dest/self.target()`.
    ///
    /// This solves a specific problem: symlink at path `src` points to an
    /// artifact at path `src/target`. We move the symlink to `dest`, but we
    /// want to keep linking to the same artifact. How can we adjust the target
    /// in order to achieve that? We need to know how to get to `src` from
    /// `dest`, which is what `src_relative_to_dest` tells us.
    pub fn relativized<P: AsRef<RelativePath>>(&self, src_relative_to_dest: P) -> Self {
        // FIXME(rafaelc): we don't need to normalize the target anymore!
        let relativized_t = src_relative_to_dest.as_ref().join_normalized(&self.0);
        Self(relativized_t)
    }
}

pub fn new_symlink<T: AsRef<Path>>(target: T) -> anyhow::Result<ActionDirectoryMember> {
    let target = target.as_ref();
    if target.is_absolute() {
        Ok(ActionDirectoryMember::ExternalSymlink(Arc::new(
            ExternalSymlink::new(target.to_path_buf(), None)?,
        )))
    } else {
        Ok(ActionDirectoryMember::Symlink(Arc::new(Symlink::new(
            RelativePathBuf::from_path(target).unwrap(),
        ))))
    }
}

pub fn directory_to_re_tree(directory: &impl ActionFingerprintedDirectory) -> RE::Tree {
    let children = directory
        .fingerprinted_ordered_walk()
        .without_paths()
        .filter_map(|entry| match entry {
            DirectoryEntry::Dir(d) => Some(d),
            DirectoryEntry::Leaf(..) => None,
        })
        .map(|d| ReDirectorySerializer::create_re_directory(d.fingerprinted_entries()))
        .collect();

    let root = ReDirectorySerializer::create_re_directory(directory.fingerprinted_entries());

    RE::Tree {
        root: Some(root),
        children,
    }
}

pub async fn re_directory_to_re_tree(
    directory: RE::Directory,
    client: &ManagedRemoteExecutionClient,
    use_case: RemoteExecutorUseCase,
) -> anyhow::Result<RE::Tree> {
    let mut children: Vec<RE::Directory> = vec![];
    let mut frontier = directory.directories.clone();
    while !frontier.is_empty() {
        let digests: Vec<remote_execution::TDigest> = frontier
            .into_iter()
            .filter_map(|d| d.digest)
            .map(|digest| remote_execution::TDigest {
                hash: digest.hash.clone(),
                size_in_bytes: digest.size_bytes,
                ..Default::default()
            })
            .collect();
        let mut retrieved = client
            .download_typed_blobs::<RE::Directory>(digests, use_case)
            .await?;
        frontier = retrieved
            .iter()
            .flat_map(|d| d.directories.clone())
            .collect();
        children.append(&mut retrieved);
    }
    Ok(RE::Tree {
        root: Some(directory),
        children,
    })
}

/// Constructs a `Directory` from an `RE::Tree`. As long as the
/// `RE::Tree` is valid (i.e. nothing is broken in the RE side), this
/// should always succeed.
#[allow(clippy::trivially_copy_pass_by_ref)] // SystemTime is a different size on Windows
pub fn re_tree_to_directory(
    tree: &RE::Tree,
    leaf_expires: &DateTime<Utc>,
    digest_config: DigestConfig,
) -> anyhow::Result<ActionDirectoryBuilder> {
    /// A map of digests to directories, populated lazily when we access it based on the hash we
    /// use. We need this because in a RE tree, the directories in the tree don't carry their hash,
    /// but the pointers are hashes, so we need to first see a hash before we can work out what
    /// hashing mechanism to use here.
    struct DirMap<'a> {
        by_kind: SmallMap<DigestAlgorithm, HashMap<FileDigest, &'a RE::Directory>>,
        directories: &'a [RE::Directory],
    }

    impl<'a> DirMap<'a> {
        fn new(directories: &'a [RE::Directory]) -> Self {
            Self {
                by_kind: SmallMap::new(),
                directories,
            }
        }

        fn get(&mut self, digest: &FileDigest) -> Option<&'a RE::Directory> {
            let algo = digest.raw_digest().algorithm();

            let map = self.by_kind.entry(algo).or_insert_with(|| {
                self.directories
                    .iter()
                    .map(|d| (from_proto_message(d, algo), d))
                    .collect()
            });

            map.get(digest).copied()
        }
    }

    fn from_proto_message<M: prost::Message>(m: &M, algo: DigestAlgorithm) -> FileDigest {
        let mut m_encoded = Vec::new();
        m.encode(&mut m_encoded)
            .unwrap_or_else(|e| unreachable!("Protobuf messages are always encodeable: {}", e));
        FileDigest::from_content_for_algorithm(m_encoded.as_slice(), algo)
    }

    // Recursively builds the directory
    fn dfs_build<'a>(
        re_dir: &'_ RE::Directory,
        re_dir_name: &'_ (impl fmt::Display + ?Sized),
        dirmap: &'_ mut DirMap<'a>,
        leaf_expires: &DateTime<Utc>,
        digest_config: DigestConfig,
    ) -> anyhow::Result<ActionDirectoryBuilder> {
        let mut builder = ActionDirectoryBuilder::empty();
        for node in &re_dir.files {
            let name = FileNameBuf::try_from(node.name.clone()).with_context(|| {
                DirectoryReConversionError::IncorrectFileName {
                    name: node.name.clone(),
                    dir: re_dir_name.to_string(),
                }
            })?;

            let digest = node.digest.as_ref().with_context(|| {
                DirectoryReConversionError::NodeWithDigestNone {
                    name: node.name.clone(),
                    dir: re_dir_name.to_string(),
                }
            })?;
            let digest = FileDigest::from_grpc(digest, digest_config)?;
            let digest = TrackedFileDigest::new_expires(
                digest,
                *leaf_expires,
                digest_config.cas_digest_config(),
            );

            let member = ActionDirectoryMember::File(FileMetadata {
                digest,
                is_executable: node.is_executable,
            });

            builder.insert(name, DirectoryEntry::Leaf(member))?;
        }
        for node in &re_dir.symlinks {
            builder.insert(
                FileNameBuf::try_from(node.name.clone()).map_err(|_| {
                    DirectoryReConversionError::IncorrectFileName {
                        name: node.name.clone(),
                        dir: re_dir_name.to_string(),
                    }
                })?,
                DirectoryEntry::Leaf(node.try_into()?),
            )?;
        }
        for dir_node in &re_dir.directories {
            let child_digest = match &dir_node.digest {
                None => {
                    return Err(DirectoryReConversionError::NodeWithDigestNone {
                        name: dir_node.name.clone(),
                        dir: re_dir_name.to_string(),
                    }
                    .into());
                }
                Some(d) => d,
            };
            let child_digest = FileDigest::from_grpc(child_digest, digest_config)?;
            let child_re_dir = match dirmap.get(&child_digest) {
                None => {
                    return Err(DirectoryReConversionError::IncompleteTreeChildrenList {
                        name: dir_node.name.clone(),
                        dir: re_dir_name.to_string(),
                    }
                    .into());
                }
                Some(d) => d,
            };
            let dir = dfs_build(
                child_re_dir,
                &child_digest,
                dirmap,
                leaf_expires,
                digest_config,
            )?;
            builder.insert(
                FileNameBuf::try_from(dir_node.name.clone()).map_err(|_| {
                    DirectoryReConversionError::IncorrectFileName {
                        name: dir_node.name.clone(),
                        dir: re_dir_name.to_string(),
                    }
                })?,
                DirectoryEntry::Dir(dir),
            )?;
        }

        // NOTE: We re-digest the directories we just received here (instead of trusting RE with
        // the hashes). But, since output directories tend to be small, that doesn't actually
        // matter.
        Ok(builder)
    }

    let root_dir = match &tree.root {
        Some(d) => d,
        None => return Ok(ActionDirectoryBuilder::empty()),
    };

    dfs_build(
        root_dir,
        "root directory",
        &mut DirMap::new(&tree.children),
        leaf_expires,
        digest_config,
    )
}

#[derive(Debug, Error)]
pub enum DirectoryReConversionError {
    // Conversion from RE::Tree errors (these shouldn't happen unless something is broken on RE side)
    #[error("Converting RE::Tree to Directory, dir `{dir}` has child `{name}` with digest=None.")]
    NodeWithDigestNone { name: String, dir: String },
    #[error(
        "Converting RE::Tree to Directory, dir `{dir}` has child `{name}` not in tree.children."
    )]
    IncompleteTreeChildrenList { name: String, dir: String },
    #[error("Converting RE::Tree to Directory, dir `{dir}` has incorrect file name `{name}`")]
    IncorrectFileName { name: String, dir: String },
}

impl<'a> TryFrom<&'a RE::SymlinkNode> for ActionDirectoryMember {
    type Error = anyhow::Error;

    fn try_from(node: &'a RE::SymlinkNode) -> Result<Self, Self::Error> {
        let symlink = if node.target.starts_with('/') {
            ActionDirectoryMember::ExternalSymlink(Arc::new(ExternalSymlink::new(
                PathBuf::from(node.target.as_str()),
                None,
            )?))
        } else {
            ActionDirectoryMember::Symlink(Arc::new(Symlink(RelativePathBuf::from(
                node.target.clone(),
            ))))
        };
        Ok(symlink)
    }
}

pub fn relativize_directory(
    builder: &mut ActionDirectoryBuilder,
    orig_root: &ProjectRelativePath,
    new_root: &ProjectRelativePath,
) -> anyhow::Result<()> {
    let mut replacements = ActionDirectoryBuilder::empty();

    {
        let mut walk = builder.unordered_walk();
        while let Some((path, entry)) = walk.next() {
            let link = match entry {
                DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(link)) => link,
                _ => continue,
            };

            let path = path.get();

            let orig_path = orig_root.join_normalized(&path)?;
            let new_path = new_root.join_normalized(&path)?;

            let orig_dest = orig_path
                .parent()
                .context("Symlink has no dir parent")?
                .join_normalized(link.target())?;

            let new_dest = new_path
                .parent()
                .context("Symlink has no dir parent")?
                .as_forward_relative_path()
                .as_relative_path()
                .relative(orig_dest);
            let new_link = Arc::new(Symlink::new(new_dest));

            replacements.insert(
                &path,
                DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(new_link)),
            )?;
        }
    }

    builder.merge(replacements)?;

    Ok(())
}

pub fn insert_entry(
    builder: &mut ActionDirectoryBuilder,
    path: &ProjectRelativePath,
    entry: ActionDirectoryEntry<ActionDirectoryBuilder>,
) -> anyhow::Result<()> {
    if let DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(s)) = entry {
        // ExternalSymlink is a bit of an odd concept. The way it works is that when you try to
        // read `foo/bar` and it turns out `foo` is an ExternalSymlink, we tell you the value of
        // `foo/bar` is an `ExternalSymlink` to some location with the "rest of the path" being
        // `bar`.

        // Later, when you try to add this to an action directory (this code), we need to put a
        // symlink at `foo` but all we have is the path this `ExternalSymlink` piece of data exists
        // at (`foo/bar`), and the "rest" of the path (`bar`), so we strip the rest of the path and
        // conclude the symlink must be at `foo` and should point at the external symlink's path
        // without the "rest" component.

        // This is kinda wonky though it sort of works. However, it does not work when we try to
        // read a symlink that points through an external symlink, e.g. reading `bar` in those
        // circumstances:

        // ```
        // bar -> foo/xxx
        // foo -> /home/blah
        // ```

        // When this happens, the "value" of `bar` is `ExternalSymlink(..., rest = "xxx")`, but the
        // path is `bar`, and we can't infer that the path must actually be `bar` and that the
        // symlink should be `/home/blah/xxx` (so in this case we keep the rest).

        // NOTE (torozco): I think we *could* perhaps make this work by using `path` as-is if we
        // can't "fix" it, and using the full symlink destination instead of
        // `s.without_remaining_path()`, but considering this doesn't seem to be something very
        // widespread and I might be missing something, I did not do it.
        let fixed_source_path = s.fix_source_path(path.as_ref()).with_context(|| {
            format!("Error locating source path for symlink at {}: {}", path, s)
        })?;
        let path = ProjectRelativePath::unchecked_new(fixed_source_path.as_str());
        let entry = DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(
            s.without_remaining_path(),
        ));
        builder.insert(path, entry)?;
    } else {
        builder.insert(path, entry)?;
    }

    Ok(())
}

/// Inserts an input to the tree, which will be required when following
/// symlinks to calculate the `deps` of the `ArtifactValue`.
pub fn insert_artifact(
    builder: &mut ActionDirectoryBuilder,
    path: &ProjectRelativePath,
    value: &ArtifactValue,
) -> anyhow::Result<()> {
    insert_entry(
        builder,
        path,
        value.entry().dupe().map_dir(|d| d.into_builder()),
    )?;
    // add input's deps
    if let Some(deps) = value.deps() {
        builder.merge(deps.dupe().into_builder())?;
    }
    Ok(())
}

pub fn insert_file(
    builder: &mut ActionDirectoryBuilder,
    path: &ProjectRelativePath,
    meta: FileMetadata,
) -> anyhow::Result<()> {
    insert_entry(
        builder,
        path,
        DirectoryEntry::Leaf(ActionDirectoryMember::File(meta)),
    )
}

#[cfg(test)]
pub fn insert_symlink(
    builder: &mut ActionDirectoryBuilder,
    path: &ProjectRelativePath,
    symlink: Arc<Symlink>,
) -> anyhow::Result<()> {
    insert_entry(
        builder,
        path,
        DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(symlink)),
    )
}

pub fn expand_selector_for_dependencies(
    builder: &ActionDirectoryBuilder,
    paths_to_take: &mut DirectorySelector,
) {
    // Note that we receive the list of paths to take as an input and start from that for the paths
    // to visit. Rhis ensures that if we add symlinks that point into the paths we are about to
    // take out, we don't bother visiting them individually, since we'll start by visiting the whole
    // thing.
    let mut paths_to_visit = paths_to_take.clone();

    let mut all_known_symlinks = HashSet::new();

    while !paths_to_visit.is_empty() {
        let mut next_paths_to_visit = DirectorySelector::empty();

        let mut handle_link_at_path = |link_path: &ForwardRelativePath, link: &Symlink| {
            let link_dst = match link_path.parent() {
                Some(parent) => parent.join_system(&link.target().to_path("")),
                None => ForwardRelativePath::new(link.target().as_str()).map(ToOwned::to_owned),
            };

            let dest = match link_dst {
                Ok(d) => d,
                Err(e) => {
                    // If we get here the symlink we found points to something that is not
                    // relative to this directory so we bail.
                    tracing::trace!(
                        "Invalid symlink: {} -> {}: {:#}",
                        link_path,
                        link.target(),
                        e
                    );
                    if cfg!(test) {
                        panic!("Invalid symlink found")
                    }
                    return;
                }
            };

            if all_known_symlinks.insert(dest.clone()) {
                tracing::trace!("New symlink: {} -> {}", link_path, dest);
                next_paths_to_visit.select(&dest);
                paths_to_take.select(&dest);
            }
        };

        let mut search = paths_to_visit.unordered_search(builder);

        while let Some((entry_path, entry)) = search.next() {
            // We're searching for all paths that are symlink destinations to find _new_ symlinks.
            // We might encounter paths that don't actually work while we do this. If that happens,
            // then we might be in a situation where we were trying to traverse *through* a
            // symlink. We check for that and handle the newly-discovered symlink if that happens.
            let mut entry_walk = match entry {
                Ok(entry) => unordered_entry_walk(entry),
                Err(e) => {
                    let link = match e.into_leaf() {
                        ActionDirectoryMember::Symlink(link) => link,
                        e => {
                            tracing::trace!(
                                "Ignoring entry to visit at: {}: cannot traverse through: {:?}",
                                entry_path,
                                e,
                            );

                            continue;
                        }
                    };

                    handle_link_at_path(&entry_path.get(), link);
                    continue;
                }
            };

            while let Some((sub_entry_path, sub_entry)) = entry_walk.next() {
                let link = match sub_entry {
                    DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(link)) => link,
                    _ => continue,
                };

                let link_path = entry_path.get().join(sub_entry_path.get());
                handle_link_at_path(&link_path, link);
            }
        }

        paths_to_visit = next_paths_to_visit;
    }
}

/// Given a builder and a Path, return it as an ArtifactValue. Dependencies are produced by
/// extracting any symlinks found in the builder.
pub fn extract_artifact_value(
    builder: &ActionDirectoryBuilder,
    path: &ProjectRelativePath,
    digest_config: DigestConfig,
) -> anyhow::Result<Option<ArtifactValue>> {
    let entry = match find(builder, path.as_forward_relative_path())? {
        Some(entry) => entry,
        _ => return Ok(None),
    };

    // This will be the set of all paths referenced by symlinks in our directory.
    let mut paths_to_take = DirectorySelector::empty();
    paths_to_take.select(path);

    expand_selector_for_dependencies(builder, &mut paths_to_take);

    let mut deps = ActionDirectoryBuilder::empty();
    let mut has_deps = false;

    for (entry_path, entry) in paths_to_take.unordered_search(builder).with_paths() {
        let entry = match entry {
            Ok(entry) => entry,
            Err(err) => {
                // We'll get here if we have a symlink that points at a path that traverses another
                // symlink (the my_genrule10 testcase has an example of this).
                tracing::trace!(
                    "Taking entry as a symlink traverses through it: {}",
                    entry_path
                );
                DirectoryEntry::Leaf(err.into_leaf())
            }
        };

        if entry_path == path.as_forward_relative_path() {
            continue;
        }

        has_deps = true;

        // TODO (@torozco): We could improve this a lot by sharing() those entries in place when we
        // extract them.
        deps.insert(
            &entry_path,
            entry.map_leaf(|l| l.dupe()).map_dir(|d| d.to_builder()),
        )?;
    }

    let entry = entry.map_leaf(|l| l.dupe()).map_dir(|d| {
        d.to_builder()
            .fingerprint(digest_config.as_directory_serializer())
            .shared(&*INTERNER)
    });

    let deps = if has_deps {
        Some(
            deps.fingerprint(digest_config.as_directory_serializer())
                .shared(&*INTERNER),
        )
    } else {
        None
    };

    Ok(Some(ArtifactValue::new(entry, deps)))
}

#[cfg(test)]
mod tests {
    use anyhow::Context as _;

    use super::*;

    fn path<'a>(s: &'a str) -> &'a ProjectRelativePath {
        ProjectRelativePath::new(s).unwrap()
    }

    fn assert_dirs_eq(d1: &impl ActionDirectory, d2: &impl ActionDirectory) {
        let mut w1 = d1.ordered_walk();
        let mut w2 = d2.ordered_walk();

        loop {
            let n1 = w1.next();
            let n2 = w2.next();

            let ((p1, e1), (p2, e2)) = match (n1, n2) {
                (Some(n1), Some(n2)) => (n1, n2),
                (Some((p1, _)), None) => {
                    panic!("Walks differ, first difference at: {}", p1);
                }
                (None, Some((p2, _))) => {
                    panic!("Walks differ, first difference at: {}", p2);
                }
                (None, None) => break,
            };

            // Compare paths
            assert_eq!(p1.get(), p2.get(), "Walks produced different paths");

            // Compare leaves
            assert_eq!(
                e1.map_dir(|_| ()),
                e2.map_dir(|_| ()),
                "Walks differ at path {}",
                p1
            );
        }
    }

    #[test]
    fn directory_relativized() -> anyhow::Result<()> {
        let digest_config = DigestConfig::testing_default();

        let mut dir = {
            let mut builder = ActionDirectoryBuilder::empty();
            insert_file(
                &mut builder,
                path("inter/f"),
                FileMetadata::empty(digest_config.cas_digest_config()),
            )?;
            insert_symlink(
                &mut builder,
                path("sym"),
                Arc::new(Symlink::new("inter/f".into())),
            )?;
            insert_symlink(
                &mut builder,
                path("esym"),
                Arc::new(Symlink::new("../f".into())),
            )?;
            builder
        };

        let expected_dir = {
            let mut builder = ActionDirectoryBuilder::empty();
            insert_file(
                &mut builder,
                path("inter/f"),
                FileMetadata::empty(digest_config.cas_digest_config()),
            )?;
            insert_symlink(
                &mut builder,
                path("sym"),
                Arc::new(Symlink::new("../a/d0/inter/f".into())),
            )?;
            insert_symlink(
                &mut builder,
                path("esym"),
                Arc::new(Symlink::new("../a/f".into())),
            )?;
            builder
        };

        // Move directory from a/d0 to b.
        relativize_directory(&mut dir, path("a/d0"), path("b"))?;

        assert_dirs_eq(&dir, &expected_dir);

        Ok(())
    }

    fn build_test_dir() -> anyhow::Result<ActionDirectoryBuilder> {
        let digest_config = DigestConfig::testing_default();

        let mut builder = ActionDirectoryBuilder::empty();
        // /
        // |-f1
        // |-d1/
        // | |-d2/
        // | | |-d3/
        // | | | |-s1 -> ../../../d6/s4          d3_deps:[s4, f4]
        // | | | |-s2 -> ../d4                   d3_deps:[d4/*]
        // | | | |-s3 -> ../d5/../../../f1       d3_deps:[f1]
        // | | |-d4/
        // | | | |-f2
        // | | |-d5/
        // | | | |-f3
        // |-d6/
        // | |-s4 -> f4
        // | |-f4
        // | |-e1 -> /mnt/gvfs
        // |-f5

        for file in &["f1", "d1/d2/d4/f2", "d1/d2/d5/f3", "d6/f4", "f5"] {
            insert_file(
                &mut builder,
                path(file),
                FileMetadata::empty(digest_config.cas_digest_config()),
            )?;
        }

        for (sym, target) in &[
            ("d1/d2/d3/s1", "../../../d6/s4"),
            ("d1/d2/d3/s2", "../d4"),
            ("d1/d2/d3/s3", "../d5/../../../f1"),
            ("d6/s4", "f4"),
            ("d6/e1", "/mnt/gvfs"),
        ] {
            insert_symlink(
                &mut builder,
                path(sym),
                Arc::new(Symlink::new(target.into())),
            )?;
        }

        Ok(builder)
    }

    /// All deps of d6 are internal to it, so we don't return any deps.
    #[test]
    fn test_extract_no_deps() -> anyhow::Result<()> {
        let digest_config = DigestConfig::testing_default();
        let root = build_test_dir()?;
        let value =
            extract_artifact_value(&root, path("d6"), digest_config)?.context("Not value!")?;
        assert!(value.deps().is_none());
        Ok(())
    }

    #[test]
    fn test_extract_has_deps_dir() -> anyhow::Result<()> {
        let digest_config = DigestConfig::testing_default();

        let root = build_test_dir()?;
        let value = extract_artifact_value(&root, path("d1/d2/d3"), digest_config)?
            .context("Not value!")?;

        let expected = {
            let mut builder = ActionDirectoryBuilder::empty();

            for p in &["d6/s4", "d6/f4", "d1/d2/d4", "f1"] {
                let path = path(p);
                let entry = find(&root, path.as_forward_relative_path())?
                    .with_context(|| format!("Missing {}", path))?
                    .map_dir(|d| d.to_builder())
                    .map_leaf(|l| l.dupe());
                insert_entry(&mut builder, path, entry)?;
            }

            builder
        };

        assert_dirs_eq(value.deps().context("No deps!")?, &expected);

        Ok(())
    }

    #[test]
    fn test_extract_has_deps_leaf() -> anyhow::Result<()> {
        let digest_config = DigestConfig::testing_default();

        let root = build_test_dir()?;
        let value = extract_artifact_value(&root, path("d1/d2/d3/s3"), digest_config)?
            .context("Not value!")?;

        let expected = {
            let mut builder = ActionDirectoryBuilder::empty();
            insert_file(
                &mut builder,
                path("f1"),
                FileMetadata::empty(digest_config.cas_digest_config()),
            )?;
            builder
        };

        assert_dirs_eq(value.deps().context("No deps!")?, &expected);

        Ok(())
    }

    #[test]
    fn test_extract_cycle() -> anyhow::Result<()> {
        let digest_config = DigestConfig::testing_default();

        let mut builder = ActionDirectoryBuilder::empty();

        for (sym, target) in &[("d1/f1", "../d2/f2"), ("d2/f2", "../d1/f1")] {
            insert_symlink(
                &mut builder,
                path(sym),
                Arc::new(Symlink::new(target.into())),
            )?;
        }

        let expected = {
            let mut builder = ActionDirectoryBuilder::empty();
            insert_symlink(
                &mut builder,
                path("d2/f2"),
                Arc::new(Symlink::new("../d1/f1".into())),
            )?;
            builder
        };

        let value = extract_artifact_value(&builder, path("d1/f1"), digest_config)?
            .context("Not value!")?;

        assert_dirs_eq(value.deps().context("No deps!")?, &expected);

        Ok(())
    }

    #[test]
    fn test_extract_symlink_chain() -> anyhow::Result<()> {
        // Crank up the difficulty: l1 points through d3/f, but through l2. We need all of those in
        // the deps! In practice, this tends to not happen in Buck 2 because we always dereference
        // symlinks and traverse them, but might a well support it properly.
        let digest_config = DigestConfig::testing_default();

        let mut builder = ActionDirectoryBuilder::empty();
        insert_symlink(
            &mut builder,
            path("l1"),
            Arc::new(Symlink::new("l2/f".into())),
        )?;

        insert_symlink(
            &mut builder,
            path("l2"),
            Arc::new(Symlink::new("d3".into())),
        )?;

        insert_file(
            &mut builder,
            path("d3/f"),
            FileMetadata::empty(digest_config.cas_digest_config()),
        )?;

        let value =
            extract_artifact_value(&builder, path("l1"), digest_config)?.context("Not value!")?;

        let expected = {
            let mut builder = ActionDirectoryBuilder::empty();

            insert_symlink(
                &mut builder,
                path("l2"),
                Arc::new(Symlink::new("d3".into())),
            )?;

            insert_file(
                &mut builder,
                path("d3/f"),
                FileMetadata::empty(digest_config.cas_digest_config()),
            )?;

            builder
        };

        assert_dirs_eq(value.deps().context("No deps!")?, &expected);

        Ok(())
    }

    #[test]
    fn test_re_tree_roundtrip() -> anyhow::Result<()> {
        let digest_config = DigestConfig::testing_default();

        let mut builder = ActionDirectoryBuilder::empty();
        insert_file(
            &mut builder,
            path("a/aa"),
            FileMetadata::empty(digest_config.cas_digest_config()),
        )?;
        insert_file(
            &mut builder,
            path("a/bb"),
            FileMetadata::empty(digest_config.cas_digest_config()),
        )?;
        insert_file(
            &mut builder,
            path("b/b"),
            FileMetadata::empty(digest_config.cas_digest_config()),
        )?;
        let dir = builder.fingerprint(digest_config.as_directory_serializer());

        let tree = directory_to_re_tree(&dir);
        let dir2 = re_tree_to_directory(&tree, &Utc::now(), digest_config)?;

        assert_dirs_eq(&dir, &dir2);

        Ok(())
    }

    /// Ensure that we serialize trees the same way RE does. The expected hash was obtained by
    /// running:
    ///
    /// ```sh
    /// buck2 run fbcode//remote_execution/rust/recli:recli -- \
    ///     exec command --out-dir test -- sh -c \
    ///     'mkdir -p test/a/aa test/a/aaa test/b/bb test/d && touch test/a/aa/f test/a/aaa/f test/b/bb/f test/d/f'
    /// ```
    #[test]
    fn test_re_tree_compatibility() -> anyhow::Result<()> {
        let digest_config = DigestConfig::testing_default();

        let mut builder = ActionDirectoryBuilder::empty();
        for p in &["a/aa/f", "a/aaa/f", "b/bb/f", "d/f"] {
            insert_file(
                &mut builder,
                path(p),
                FileMetadata::empty(digest_config.cas_digest_config()),
            )?;
        }
        let dir = builder.fingerprint(digest_config.as_directory_serializer());
        let tree = directory_to_re_tree(&dir);
        let tree = proto_serialize(&tree);
        let digest = FileDigest::from_content(&tree, digest_config.cas_digest_config());
        assert_eq!(
            digest.to_string(),
            "28e2632abbdc186ddd3ce26d2faf2da40c4c2695:521"
        );

        Ok(())
    }

    #[cfg(target_os = "windows")]
    #[test]
    //Test that a symlink created with a windows path doesn't get interpreted as an invalid sylink
    //TODO(lmvasquezg) Update symlinks to store a normalized, OS-independent path
    fn test_unnormalized_symlinks() -> anyhow::Result<()> {
        let digest_config = DigestConfig::testing_default();

        let mut builder = ActionDirectoryBuilder::empty();
        insert_symlink(
            &mut builder,
            path("d1/f1"),
            Arc::new(Symlink::new("..\\d2\\f2".into())),
        )?;
        insert_file(
            &mut builder,
            path("d2/f2"),
            FileMetadata::empty(digest_config.cas_digest_config()),
        )?;

        extract_artifact_value(&builder, path("d1/f1"), digest_config)?.context("Not value!")?;
        Ok(())
    }
}
