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
use std::convert::TryInto;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use async_recursion::async_recursion;
use async_trait::async_trait;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::query::dice::get_compatible_targets;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::file_ops::FileOps;
use buck2_common::file_ops::PathMetadata;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::package::Package;
use buck2_core::result::SharedResult;
use buck2_core::target::TargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::environment::ConfiguredOrUnconfiguredTargetLabel;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use buck2_query::query::traversal::AsyncNodeLookup;
use buck2_query::query::traversal::AsyncTraversalDelegate;
use buck2_query::query::traversal::ChildVisitor;
use cli_proto::targets_request::TargetHashFileMode;
use dice::DiceComputations;
use dice::DiceTransaction;
use futures::future::join_all;
use futures::future::BoxFuture;
use futures::future::Shared;
use futures::join;
use futures::stream::FuturesUnordered;
use futures::FutureExt;
use futures::StreamExt;
use gazebo::prelude::*;
use os_str_bytes::OsStrBytes;
use siphasher::sip128::Hasher128;
use siphasher::sip128::SipHasher24;

#[derive(Clone, Dupe)]
pub(crate) struct BuckTargetHash(pub u128);

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
    ctx: DiceTransaction,
}

#[async_trait]
impl FileHasher for PathsAndContentsHasher {
    async fn hash_path(&self, cell_path: &CellPath) -> anyhow::Result<Vec<u8>> {
        #[async_recursion]
        async fn hash_item(
            file_ops: &dyn FileOps,
            cell_path: &CellPath,
            res: &mut Vec<u8>,
        ) -> anyhow::Result<()> {
            let info = file_ops.read_path_metadata(cell_path).await?;
            // Important that the different branches can never clash, so add a prefix byte to them
            match info {
                PathMetadata::File(m) => {
                    res.reserve(1 + m.digest.sha1().len());
                    // We ignore `digest.size` as the SHA1 alone is enough to be unique
                    res.push(0u8);
                    res.extend(m.digest.sha1());
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
                    let files = file_ops.read_dir(cell_path).await?;
                    res.extend(&files.len().to_be_bytes());
                    for x in &*files {
                        let name = x.file_name.as_str();
                        res.extend(&name.len().to_be_bytes());
                        res.extend(name.as_bytes());
                        hash_item(file_ops, &cell_path.join_unnormalized(&x.file_name), res)
                            .await?;
                    }
                }
            }
            Ok(())
        }

        let file_ops = self.ctx.file_ops();
        let mut res = Vec::new();
        hash_item(&file_ops, cell_path, &mut res).await?;
        Ok(res)
    }
}

#[async_trait]
pub(crate) trait TargetHashingTargetNode: QueryTarget + Hash {
    // Get the (un)Configured Target Node from a (un)Configured Target Label.
    async fn get_from_ctx(ctx: &DiceTransaction, label: &Self::NodeRef) -> anyhow::Result<Self>;

    // Takes in Target Nodes and returns a new set of (un)Configured
    // Target Nodes based on type of hashing specified.
    async fn get_target_nodes(
        ctx: &DiceComputations,
        loaded_targets: impl Iterator<Item = (&Package, SharedResult<Vec<TargetNode>>)>
        + std::marker::Send,
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<TargetSet<Self>>;
}

#[async_trait]
impl TargetHashingTargetNode for ConfiguredTargetNode {
    async fn get_from_ctx(ctx: &DiceTransaction, label: &Self::NodeRef) -> anyhow::Result<Self> {
        Ok(ctx
            .get_configured_target_node(label)
            .await?
            .require_compatible()?)
    }
    async fn get_target_nodes(
        ctx: &DiceComputations,
        loaded_targets: impl Iterator<Item = (&Package, SharedResult<Vec<TargetNode>>)>
        + std::marker::Send,
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<TargetSet<Self>> {
        get_compatible_targets(ctx, loaded_targets, global_target_platform).await
    }
}

#[async_trait]
impl TargetHashingTargetNode for TargetNode {
    async fn get_from_ctx(ctx: &DiceTransaction, label: &Self::NodeRef) -> anyhow::Result<Self> {
        Ok(ctx.get_target_node(label).await?)
    }

    async fn get_target_nodes(
        _ctx: &DiceComputations,
        loaded_targets: impl Iterator<Item = (&Package, SharedResult<Vec<TargetNode>>)>
        + std::marker::Send,
        _global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<TargetSet<Self>> {
        let mut target_set = TargetSet::new();
        for (_package, result) in loaded_targets {
            let targets = result?;
            for target in targets {
                target_set.insert(target);
            }
        }
        Ok(target_set)
    }
}
pub(crate) struct TargetHashes {
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
    pub(crate) fn get(&self, label: &TargetLabel) -> Option<SharedResult<BuckTargetHash>> {
        return self.target_mapping.get(label).cloned();
    }

    pub(crate) async fn compute<T: TargetHashingTargetNode>(
        ctx: DiceTransaction,
        targets: Vec<(&Package, SharedResult<Vec<TargetNode>>)>,
        global_target_platform: Option<TargetLabel>,
        file_hash_mode: TargetHashFileMode,
        modified_paths: HashSet<CellPath>,
        use_fast_hash: bool,
    ) -> anyhow::Result<Self>
    where
        T::NodeRef: ConfiguredOrUnconfiguredTargetLabel,
    {
        struct Lookup {
            ctx: DiceTransaction,
        }

        #[async_trait]
        impl<T: TargetHashingTargetNode> AsyncNodeLookup<T> for Lookup {
            async fn get(&self, label: &T::NodeRef) -> anyhow::Result<T> {
                T::get_from_ctx(&self.ctx, label).await
            }
        }

        struct Delegate<T: QueryTarget> {
            hashes: HashMap<T::NodeRef, Shared<BoxFuture<'static, SharedResult<BuckTargetHash>>>>,
            file_hasher: Arc<dyn FileHasher>,
            use_fast_hash: bool,
        }

        #[async_trait]
        impl<T: Hash + QueryTarget> AsyncTraversalDelegate<T> for Delegate<T> {
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

                let use_fast_hash = self.use_fast_hash;
                // we spawn off the hash computation since it can't be done in visit directly. Even if it could,
                // this allows us to start the computations for dependents before finishing the computation for a node.
                self.hashes.insert(
                    target.node_ref().clone(),
                    async move {
                        tokio::spawn(async move {
                            let mut hasher = TargetHashes::new_hasher(use_fast_hash);
                            TargetHashes::hash_node(&target, &mut *hasher)?;

                            let mut input_futs = Vec::new();
                            target.inputs_for_each(|cell_path| {
                                let file_hasher = file_hasher.dupe();
                                input_futs.push(async move {
                                    let file_hash = file_hasher.hash_path(&cell_path).await;
                                    (cell_path, file_hash)
                                });
                                anyhow::Ok(())
                            })?;

                            let (dep_hashes, input_hashes) =
                                join!(join_all(dep_futures), join_all(input_futs));

                            TargetHashes::hash_deps(dep_hashes, &mut *hasher)?;
                            TargetHashes::hash_files(input_hashes, &mut *hasher)?;

                            Ok(hasher.finish_u128())
                        })
                        .await
                        .unwrap()
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

        let targets =
            T::get_target_nodes(&ctx, targets.into_iter(), global_target_platform).await?;

        let lookup = Lookup { ctx: ctx.dupe() };
        let file_hasher: Arc<dyn FileHasher> = match file_hash_mode {
            TargetHashFileMode::PathsOnly => Arc::new(PathsOnlyFileHasher {
                pseudo_changed_paths: modified_paths,
            }),
            TargetHashFileMode::PathsAndContents => Arc::new(PathsAndContentsHasher { ctx }),
        };

        let mut delegate = Delegate::<T> {
            hashes: HashMap::new(),
            file_hasher,
            use_fast_hash,
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
                    target.unconfigured_label().name().value()
                );
            }
        }
        Ok(Self { target_mapping })
    }

    fn new_hasher(use_fast_hash: bool) -> Box<dyn BuckTargetHasher> {
        if use_fast_hash {
            box SipHasher24::new()
        } else {
            box Blake3Adapter(blake3::Hasher::new())
        }
    }

    fn hash_node<T: Hash + QueryTarget>(
        node: &T,
        mut hasher: &mut dyn BuckTargetHasher,
    ) -> SharedResult<()> {
        node.hash(&mut hasher);
        Ok(())
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
