/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    collections::{HashMap, HashSet},
    convert::TryInto,
    hash::{Hash, Hasher},
    sync::Arc,
};

use async_recursion::async_recursion;
use async_trait::async_trait;
use buck2_build_api::{
    calculation::Calculation,
    nodes::{configured::ConfiguredTargetNode, unconfigured::TargetNode},
    query::dice::get_compatible_targets,
};
use buck2_common::{
    dice::file_ops::HasFileOps,
    file_ops::{FileOps, PathMetadata},
};
use buck2_core::{
    cells::paths::CellPath,
    package::Package,
    result::SharedResult,
    target::{ConfiguredTargetLabel, TargetLabel},
};
use buck2_query::query::traversal::{
    async_depth_first_postorder_traversal, AsyncNodeLookup, AsyncTraversalDelegate, ChildVisitor,
};
use cli_proto::targets_request::TargetHashFileMode;
use dice::DiceTransaction;
use futures::{
    future::{join_all, BoxFuture, Shared},
    join,
    stream::FuturesUnordered,
    FutureExt, StreamExt,
};
use gazebo::prelude::*;
use os_str_bytes::OsStrBytes;
use siphasher::sip128::{Hasher128, SipHasher24};

#[derive(Clone, Dupe)]
pub enum BuckTargetHash {
    Compatible(u128),
    Incompatible,
}

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
        BuckTargetHash::Compatible(self.finish128().as_u128())
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
        BuckTargetHash::Compatible(u128::from_le_bytes(bytes[16..].try_into().unwrap()))
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
                    res.reserve(1 + m.digest.sha1.len());
                    // We ignore `digest.size` as the SHA1 alone is enough to be unique
                    res.push(0u8);
                    res.extend(m.digest.sha1);
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

pub struct TargetHashes {
    compatible_target_mapping: HashMap<TargetLabel, ConfiguredTargetLabel>,
    // TODO(scottcao): the typing here can be cleaned up a little
    hashes: HashMap<ConfiguredTargetLabel, SharedResult<BuckTargetHash>>,
}

impl TargetHashes {
    pub fn get(&self, label: &TargetLabel) -> Option<SharedResult<BuckTargetHash>> {
        self.compatible_target_mapping
            .get(label)
            .map(|v| match self.hashes.get(v).cloned() {
                Some(r) => r,
                None => panic!("Target {} is compatible but does not have a hash", label),
            })
    }

    pub async fn compute(
        ctx: DiceTransaction,
        targets: impl Iterator<Item = (&Package, SharedResult<Vec<TargetNode>>)>,
        global_target_platform: Option<TargetLabel>,
        file_hash_mode: TargetHashFileMode,
        modified_paths: HashSet<CellPath>,
        use_fast_hash: bool,
    ) -> anyhow::Result<Self> {
        struct Lookup {
            ctx: DiceTransaction,
        }
        #[async_trait]
        impl AsyncNodeLookup<ConfiguredTargetNode> for Lookup {
            async fn get(
                &self,
                label: &ConfiguredTargetLabel,
            ) -> anyhow::Result<ConfiguredTargetNode> {
                // TODO(scottcao): We can get ConfiguredTargetNode using `compatible_target_mapping` before
                // calling `get_configured_target_node`
                Ok(self
                    .ctx
                    .get_configured_target_node(label)
                    .await?
                    .require_compatible()?)
            }
        }

        struct Delegate {
            hashes: HashMap<
                ConfiguredTargetLabel,
                Shared<BoxFuture<'static, SharedResult<BuckTargetHash>>>,
            >,
            file_hasher: Arc<dyn FileHasher>,
            use_fast_hash: bool,
        }

        #[async_trait]
        impl AsyncTraversalDelegate<ConfiguredTargetNode> for Delegate {
            fn visit(&mut self, target: ConfiguredTargetNode) -> anyhow::Result<()> {
                // this is postorder, so guaranteed that all deps have futures already.
                let dep_futures: Vec<_> = target
                    .deps()
                    .map(|dep| self.hashes.get(dep.target()).unwrap().clone())
                    .collect();
                let file_hasher = self.file_hasher.dupe();

                let use_fast_hash = self.use_fast_hash;
                // we spawn off the hash computation since it can't be done in visit directly. Even if it could,
                // this allows us to start the computations for dependents before finishing the computation for a node.
                self.hashes.insert(
                    target.name().dupe(),
                    async move {
                        tokio::spawn(async move {
                            let mut hasher = TargetHashes::new_hasher(use_fast_hash);
                            TargetHashes::hash_node(&target, &mut *hasher)?;

                            let input_futs: Vec<_> = target
                                .inputs()
                                .map(|cell_path| {
                                    let file_hasher = file_hasher.dupe();
                                    async move {
                                        let file_hash = file_hasher.hash_path(&cell_path).await;
                                        (cell_path, file_hash)
                                    }
                                })
                                .collect();

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
                target: &ConfiguredTargetNode,
                func: &mut dyn ChildVisitor<ConfiguredTargetNode>,
            ) -> anyhow::Result<()> {
                for dep in target.deps() {
                    func.visit(dep.target().dupe())?;
                }

                Ok(())
            }
        }

        let compatible_targets =
            get_compatible_targets(&ctx, targets, global_target_platform).await?;
        let compatible_target_mapping: HashMap<_, _> = compatible_targets
            .iter()
            .map(|node| (node.name().unconfigured().dupe(), node.name().dupe()))
            .collect();

        let lookup = Lookup { ctx: ctx.dupe() };
        let file_hasher: Arc<dyn FileHasher> = match file_hash_mode {
            TargetHashFileMode::PathsOnly => Arc::new(PathsOnlyFileHasher {
                pseudo_changed_paths: modified_paths,
            }),
            TargetHashFileMode::PathsAndContents => Arc::new(PathsAndContentsHasher { ctx }),
        };

        let mut delegate = Delegate {
            hashes: HashMap::new(),
            file_hasher,
            use_fast_hash,
        };

        async_depth_first_postorder_traversal(
            &lookup,
            compatible_targets.iter_names(),
            &mut delegate,
        )
        .await?;

        let mut futures: FuturesUnordered<_> = delegate
            .hashes
            .into_iter()
            .map(|(target, fut)| async move { (target, fut.await) })
            .collect();

        let mut hashes = HashMap::new();
        // TODO(cjhopman): FuturesOrdered/Unordered interacts poorly with tokio cooperative scheduling
        // (see https://github.com/rust-lang/futures-rs/issues/2053). Clean this up once a good
        // solution there exists.
        while let Some((target, hash)) = tokio::task::unconstrained(futures.next()).await {
            hashes.insert(target, hash);
        }
        Ok(Self {
            compatible_target_mapping,
            hashes,
        })
    }

    fn new_hasher(use_fast_hash: bool) -> Box<dyn BuckTargetHasher> {
        if use_fast_hash {
            box SipHasher24::new()
        } else {
            box Blake3Adapter(blake3::Hasher::new())
        }
    }

    fn hash_node(
        node: &ConfiguredTargetNode,
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
            match target_hash? {
                BuckTargetHash::Compatible(hash) => hasher.write_u128(hash),
                _ => {}
            }
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
