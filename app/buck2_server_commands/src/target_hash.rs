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
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use async_recursion::async_recursion;
use async_trait::async_trait;
use buck2_build_api::configure_targets::get_compatible_targets;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::file_ops::FileOps;
use buck2_common::file_ops::PathMetadata;
use buck2_common::file_ops::PathMetadataOrRedirection;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::package::PackageLabel;
use buck2_core::target::label::TargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::environment::ConfiguredOrUnconfiguredTargetLabel;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use buck2_query::query::traversal::AsyncNodeLookup;
use buck2_query::query::traversal::AsyncTraversalDelegate;
use buck2_query::query::traversal::ChildVisitor;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::future::join_all;
use futures::future::BoxFuture;
use futures::future::Shared;
use futures::join;
use futures::stream::FuturesUnordered;
use futures::FutureExt;
use futures::StreamExt;
use os_str_bytes::OsStrBytes;
use siphasher::sip128::Hasher128;
use siphasher::sip128::SipHasher24;

#[derive(Clone, Dupe, derive_more::Display)]
#[display(fmt = "{:032x}", _0)]
pub struct BuckTargetHash(pub u128);

trait BuckTargetHasher: Hasher + Send + 'static {
    fn finish_u128(&mut self) -> BuckTargetHash;
}

/// siphash24 is used as the "fast" hash. There are faster hash algorithms out there,
/// but many (xxhash, ahash, etc) are poorly documented and having deterministic behavior
/// across architectures is rarely guaranteed. We see about a 20-25% improvement relative
/// to blake3 and so there's likely little opportunity remaining for a faster hash function
/// to capture anyway.
impl BuckTargetHasher for siphasher::sip128::SipHasher24 {
    fn finish_u128(&mut self) -> BuckTargetHash {
        BuckTargetHash(self.finish128().as_u128())
    }
}

/// We use blake3 as our "strong" hash.
struct Blake3Adapter(blake3::Hasher);

impl Hasher for Blake3Adapter {
    fn finish(&self) -> u64 {
        unimplemented!()
    }

    fn write(&mut self, bytes: &[u8]) {
        self.0.update(bytes);
    }
}

impl BuckTargetHasher for Blake3Adapter {
    fn finish_u128(&mut self) -> BuckTargetHash {
        let hash = blake3::Hasher::finalize(&self.0);
        let bytes = hash.as_bytes();
        BuckTargetHash(u128::from_le_bytes(bytes[16..].try_into().unwrap()))
    }
}

pub enum TargetHashesFileMode {
    /// The following files have changed in some way (don't do any IO)
    PathsOnly(HashSet<CellPath>),
    /// Use IO operations to find the paths and their contents
    PathsAndContents,
    /// Don't hash any files
    None,
}

#[async_trait]
trait FileHasher: Send + Sync {
    /// Obtain information about a path in some manner.
    async fn hash_path(&self, path: &CellPath) -> anyhow::Result<Vec<u8>>;
}

struct PathsOnlyFileHasher {
    pseudo_changed_paths: HashSet<CellPath>,
}

#[async_trait]
impl FileHasher for PathsOnlyFileHasher {
    async fn hash_path(&self, path: &CellPath) -> anyhow::Result<Vec<u8>> {
        if self.pseudo_changed_paths.contains(path) {
            Ok(vec![0u8])
        } else {
            Ok(Vec::new())
        }
    }
}

struct PathsAndContentsHasher {
    dice: DiceTransaction,
}

#[async_trait]
impl FileHasher for PathsAndContentsHasher {
    async fn hash_path(&self, cell_path: &CellPath) -> anyhow::Result<Vec<u8>> {
        #[async_recursion]
        async fn hash_item(
            file_ops: &dyn FileOps,
            cell_path: CellPathRef<'async_recursion>,
            res: &mut Vec<u8>,
        ) -> anyhow::Result<()> {
            let info = file_ops.read_path_metadata(cell_path.dupe()).await?;
            // Important that the different branches can never clash, so add a prefix byte to them
            match PathMetadataOrRedirection::from(info) {
                PathMetadataOrRedirection::PathMetadata(meta) => match meta {
                    PathMetadata::File(m) => {
                        let digest = m.digest.raw_digest().as_bytes();
                        res.reserve(1 + digest.len());
                        // We ignore `digest.size` as the SHA1 alone is enough to be unique
                        res.push(0u8);
                        res.extend(digest);
                    }
                    PathMetadata::ExternalSymlink(m) => {
                        // We don't want to go to the disk and get the digest of the file in the external symlink.
                        // But we do want to change the details if the target of the symlink changes, so
                        // take the data in the symlink, put it into a buffer, and produce a digest of that
                        let target = m.to_path_buf();
                        let target = target.to_raw_bytes();
                        res.reserve(1 + target.len());
                        res.push(1u8);
                        res.extend(&*target);
                    }
                    PathMetadata::Directory => {
                        res.push(2u8);
                        let files = file_ops.read_dir(cell_path.dupe()).await?.included;
                        res.extend(files.len().to_be_bytes());
                        for x in &*files {
                            let name = x.file_name.as_str();
                            res.extend(name.len().to_be_bytes());
                            res.extend(name.as_bytes());
                            hash_item(file_ops, cell_path.join(&x.file_name).as_ref(), res).await?;
                        }
                    }
                },
                PathMetadataOrRedirection::Redirection(r) => {
                    // TODO (T126181780): This should have a limit on recursion.
                    hash_item(file_ops, r.as_ref().as_ref(), res).await?;
                }
            }
            Ok(())
        }

        let file_ops = self.dice.file_ops();
        let mut res = Vec::new();
        hash_item(&file_ops, cell_path.as_ref(), &mut res).await?;
        Ok(res)
    }
}

/// Types of node that can be target hashed (just configured and unconfigured).
/// This trait is purposely defined here instead of in buck2_node crate
/// so that we can only access the public fields of these nodes.
#[async_trait]
pub trait TargetHashingTargetNode: QueryTarget {
    /// We only hash this node, not its dependencies.
    /// Importantly, we look at the nodes after configuration (for the configured case).
    fn target_hash<H: Hasher>(&self, state: &mut H);

    // Takes in Target Nodes and returns a new set of (un)Configured
    // Target Nodes based on type of hashing specified.
    async fn get_target_nodes(
        dice: &DiceComputations,
        loaded_targets: Vec<(PackageLabel, anyhow::Result<Vec<TargetNode>>)>,
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<TargetSet<Self>>;
}

#[async_trait]
impl TargetHashingTargetNode for ConfiguredTargetNode {
    fn target_hash<H: Hasher>(&self, state: &mut H) {
        self.target_hash(state)
    }

    async fn get_target_nodes(
        dice: &DiceComputations,
        loaded_targets: Vec<(PackageLabel, anyhow::Result<Vec<TargetNode>>)>,
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<TargetSet<Self>> {
        get_compatible_targets(dice, loaded_targets.into_iter(), global_target_platform).await
    }
}

#[async_trait]
impl TargetHashingTargetNode for TargetNode {
    fn target_hash<H: Hasher>(&self, state: &mut H) {
        self.target_hash(state)
    }

    async fn get_target_nodes(
        _dice: &DiceComputations,
        loaded_targets: Vec<(PackageLabel, anyhow::Result<Vec<TargetNode>>)>,
        _global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<TargetSet<Self>> {
        let mut target_set = TargetSet::new();
        for (_package, result) in loaded_targets {
            target_set.extend(result?);
        }
        Ok(target_set)
    }
}
pub struct TargetHashes {
    // key is an unconfigured target label, but the hash is generated from the configured target label.
    target_mapping: HashMap<TargetLabel, SharedResult<BuckTargetHash>>,
}

#[derive(thiserror::Error, Debug)]
enum TargetHashError {
    #[error(
        "Found a dependency `{0}` of target `{1}` which has not been hashed yet. This may indicate a dependency cycle in the unconfigured graph."
    )]
    DependencyCycle(String, String),
}

impl TargetHashes {
    pub fn get(&self, label: &TargetLabel) -> Option<&SharedResult<BuckTargetHash>> {
        self.target_mapping.get(label)
    }

    async fn compute_recursive_target_hashes<T: TargetHashingTargetNode, L: AsyncNodeLookup<T>>(
        dice: DiceTransaction,
        lookup: L,
        targets: TargetSet<T>,
        file_hasher: Option<Arc<dyn FileHasher>>,
        use_fast_hash: bool,
    ) -> anyhow::Result<Self>
    where
        T::NodeRef: ConfiguredOrUnconfiguredTargetLabel,
    {
        struct Delegate<T: QueryTarget> {
            hashes: HashMap<T::NodeRef, Shared<BoxFuture<'static, SharedResult<BuckTargetHash>>>>,
            file_hasher: Option<Arc<dyn FileHasher>>,
            use_fast_hash: bool,
            dice: DiceTransaction,
        }

        #[async_trait]
        impl<T: TargetHashingTargetNode> AsyncTraversalDelegate<T> for Delegate<T> {
            fn visit(&mut self, target: T) -> anyhow::Result<()> {
                // this is postorder, so guaranteed that all deps have futures already.
                let dep_futures: Vec<_> = target
                    .deps()
                    .map(|dep| {
                        self.hashes.get(dep).cloned().ok_or_else(|| {
                            TargetHashError::DependencyCycle(
                                dep.clone().to_string(),
                                target.node_ref().to_string(),
                            )
                        })
                    })
                    .collect::<Result<Vec<_>, TargetHashError>>()?;

                let file_hasher = self.file_hasher.dupe();
                let dice = self.dice.dupe();

                let use_fast_hash = self.use_fast_hash;
                // we spawn off the hash computation since it can't be done in visit directly. Even if it could,
                // this allows us to start the computations for dependents before finishing the computation for a node.
                self.hashes.insert(
                    target.node_ref().clone(),
                    async move {
                        dice.temporary_spawn(move |_| async move {
                            let mut hasher = TargetHashes::new_hasher(use_fast_hash);
                            TargetHashes::hash_node(&target, &mut *hasher);

                            let mut input_futs = Vec::new();
                            if let Some(file_hasher) = file_hasher {
                                target.inputs_for_each(|cell_path| {
                                    let file_hasher = file_hasher.dupe();
                                    input_futs.push(async move {
                                        let file_hash = file_hasher.hash_path(&cell_path).await;
                                        (cell_path, file_hash)
                                    });
                                    anyhow::Ok(())
                                })?;
                            }

                            let (dep_hashes, input_hashes) =
                                join!(join_all(dep_futures), join_all(input_futs));

                            TargetHashes::hash_deps(dep_hashes, &mut *hasher)?;
                            TargetHashes::hash_files(input_hashes, &mut *hasher)?;

                            Ok(hasher.finish_u128())
                        })
                        .await
                    }
                    .boxed()
                    .shared(),
                );

                Ok(())
            }

            async fn for_each_child(
                &mut self,
                target: &T,
                func: &mut dyn ChildVisitor<T>,
            ) -> anyhow::Result<()> {
                for dep in target.deps() {
                    func.visit(dep.clone())?;
                }

                Ok(())
            }
        }

        let mut delegate = Delegate::<T> {
            hashes: HashMap::new(),
            file_hasher,
            use_fast_hash,
            dice,
        };

        async_depth_first_postorder_traversal(&lookup, targets.iter_names(), &mut delegate).await?;

        let mut futures: FuturesUnordered<_> = delegate
            .hashes
            .into_iter()
            .map(|(target, fut)| async move { (target, fut.await) })
            .collect();

        let mut target_mapping: HashMap<TargetLabel, SharedResult<BuckTargetHash>> = HashMap::new();

        // TODO(cjhopman): FuturesOrdered/Unordered interacts poorly with tokio cooperative scheduling
        // (see https://github.com/rust-lang/futures-rs/issues/2053). Clean this up once a good
        // solution there exists.

        while let Some((target, hash)) = tokio::task::unconstrained(futures.next()).await {
            // Only need the hashes of the requested target, not of dependencies.
            if targets.contains(&target) {
                let overwrite = target_mapping.insert(target.unconfigured_label().dupe(), hash);
                assert!(
                    overwrite.is_none(),
                    "Target {} was computed multiple times.",
                    target.unconfigured_label().name().as_str()
                );
            }
        }
        Ok(Self { target_mapping })
    }

    async fn compute_immediate_target_hashes<T: TargetHashingTargetNode>(
        targets: TargetSet<T>,
        file_hasher: Option<Arc<dyn FileHasher>>,
        use_fast_hash: bool,
    ) -> anyhow::Result<Self>
    where
        T::NodeRef: ConfiguredOrUnconfiguredTargetLabel,
    {
        let hashing_futures: Vec<_> = targets
            .into_iter()
            .map(|target| {
                let file_hasher = file_hasher.dupe();
                async move {
                    let hash_result: anyhow::Result<BuckTargetHash> = try {
                        let mut hasher = TargetHashes::new_hasher(use_fast_hash);
                        TargetHashes::hash_node(&target, &mut *hasher);

                        if let Some(file_hasher) = file_hasher {
                            let mut input_futs = Vec::new();
                            target.inputs_for_each(|cell_path| {
                                let file_hasher = file_hasher.dupe();
                                input_futs.push(async move {
                                    let file_hash = file_hasher.hash_path(&cell_path).await;
                                    (cell_path, file_hash)
                                });
                                anyhow::Ok(())
                            })?;

                            let input_hashes = join_all(input_futs).await;
                            TargetHashes::hash_files(input_hashes, &mut *hasher)?;
                        }

                        hasher.finish_u128()
                    };
                    (
                        target.node_ref().unconfigured_label().dupe(),
                        hash_result.shared_error(),
                    )
                }
                .boxed()
                .shared()
            })
            .collect();

        let target_mapping: HashMap<TargetLabel, SharedResult<BuckTargetHash>> =
            join_all(hashing_futures).await.into_iter().collect();
        Ok(Self { target_mapping })
    }

    pub fn compute_immediate_one(node: &TargetNode, use_fast_hash: bool) -> BuckTargetHash {
        let mut hasher = TargetHashes::new_hasher(use_fast_hash);
        TargetHashes::hash_node(node, &mut *hasher);
        hasher.finish_u128()
    }

    pub async fn compute<T: TargetHashingTargetNode, L: AsyncNodeLookup<T>>(
        dice: DiceTransaction,
        lookup: L,
        targets: Vec<(PackageLabel, anyhow::Result<Vec<TargetNode>>)>,
        global_target_platform: Option<TargetLabel>,
        file_hash_mode: TargetHashesFileMode,
        use_fast_hash: bool,
        target_hash_recursive: bool,
    ) -> anyhow::Result<Self>
    where
        T::NodeRef: ConfiguredOrUnconfiguredTargetLabel,
    {
        let targets = T::get_target_nodes(&dice, targets, global_target_platform).await?;
        let file_hasher = Self::new_file_hasher(dice.dupe(), file_hash_mode);
        if target_hash_recursive {
            Self::compute_recursive_target_hashes(dice, lookup, targets, file_hasher, use_fast_hash)
                .await
        } else {
            Self::compute_immediate_target_hashes(targets, file_hasher, use_fast_hash).await
        }
    }

    fn new_file_hasher(
        dice: DiceTransaction,
        file_hash_mode: TargetHashesFileMode,
    ) -> Option<Arc<dyn FileHasher>> {
        match file_hash_mode {
            TargetHashesFileMode::PathsOnly(modified_paths) => {
                Some(Arc::new(PathsOnlyFileHasher {
                    pseudo_changed_paths: modified_paths,
                }))
            }
            TargetHashesFileMode::PathsAndContents => {
                Some(Arc::new(PathsAndContentsHasher { dice: dice.dupe() }))
            }
            TargetHashesFileMode::None => None,
        }
    }

    fn new_hasher(use_fast_hash: bool) -> Box<dyn BuckTargetHasher> {
        if use_fast_hash {
            Box::new(SipHasher24::new())
        } else {
            Box::new(Blake3Adapter(blake3::Hasher::new()))
        }
    }

    fn hash_node<T: TargetHashingTargetNode>(node: &T, mut hasher: &mut dyn BuckTargetHasher) {
        node.target_hash(&mut hasher);
    }

    fn hash_deps(
        dep_hashes: Vec<SharedResult<BuckTargetHash>>,
        hasher: &mut dyn BuckTargetHasher,
    ) -> SharedResult<()> {
        for target_hash in dep_hashes {
            hasher.write_u128(target_hash?.0);
        }
        Ok(())
    }

    fn hash_files(
        file_digests: Vec<(CellPath, anyhow::Result<Vec<u8>>)>,
        mut hasher: &mut dyn BuckTargetHasher,
    ) -> SharedResult<()> {
        for (path, digest) in file_digests {
            path.hash(&mut hasher);
            let digest = digest?;
            hasher.write_usize(digest.len());
            hasher.write(&digest);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::target_hash::BuckTargetHash;

    #[test]
    fn test_hash_display() {
        assert_eq!(
            "00000000000000000000000000000000",
            BuckTargetHash(0).to_string()
        );
        assert_eq!(
            "ffffffffffffffffffffffffffffffff",
            BuckTargetHash(u128::MAX).to_string()
        );
    }
}
