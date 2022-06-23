/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;

use buck2_core::cells::paths::CellPath;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::package::Package;
use dice::DiceComputations;
use futures::channel::mpsc;
use futures::stream::FuturesUnordered;
use futures::Stream;
use futures::StreamExt;
use gazebo::prelude::*;
use more_futures::drop::DropTogether;
use once_cell::sync::Lazy;
use tokio::sync::Semaphore;

use crate::dice::cells::HasCellResolver;
use crate::dice::file_ops::HasFileOps;
use crate::file_ops::FileOps;
use crate::find_buildfile::find_buildfile;

/// Resolves a list of CellPath to a stream of Package representing all the
/// packages recursively contained in the paths (used for resolving patterns
/// like `//module/...`). There's no guarantees about the order that results
/// are returned, if ordering is important the caller needs to handle it.
pub async fn find_package_roots_stream(
    ctx: &DiceComputations,
    paths: Vec<CellPath>,
) -> impl Stream<Item = anyhow::Result<Package>> {
    // Ideally we wouldn't take a ctx here, but if we pull things like the package_listing_resolver
    // out of the ctx, that resolver would have a lifetime bound to the ctx and then we couldn't
    // do a tokio::spawn (or even the ctx.temporary_spawn). So, we need to only pull those things
    // out within the spawned task. This will likely need to be changed to be another trait so we
    // can write some tests.

    // TODO(cjhopman): Convert this to a inline defined stream when support for those is good.
    let (packages_tx, packages_rx) = mpsc::unbounded();

    // We don't wait on the task finishing. The packages_rx we return will naturally end when the tx side is dropped.
    let spawned = ctx.temporary_spawn(|ctx| async move {
        let file_ops = ctx.file_ops();
        let cell_resolver = ctx.get_cell_resolver().await?;
        let _ignored = find_package_roots_impl(&file_ops, &cell_resolver, paths, |res| {
            packages_tx.unbounded_send(res)
        })
        .await;

        anyhow::Ok(())
    });

    DropTogether::new(packages_rx, spawned)
}

async fn find_package_roots_impl<E>(
    file_ops: &dyn FileOps,
    cell_resolver: &CellResolver,
    paths: Vec<CellPath>,
    mut func: impl FnMut(anyhow::Result<Package>) -> Result<(), E>,
) -> Result<(), E> {
    // While we are discovering packages we may also be trying to load them. Both of these can
    // require reading dirs so we want to leave some capacity to serve the package loading.
    // We should make sure this is less than the semaphore used for limiting total read_dir.
    // TODO(cjhopman): We could probably figure out some form of tiered semaphore and tokio
    // num threads configuration to coordinate these.
    static SEMAPHORE: Lazy<Semaphore> = Lazy::new(|| Semaphore::new(300));
    let semaphore = &SEMAPHORE;

    let mut queue = FuturesUnordered::new();
    let mut seen = HashSet::new();

    let list_dir = |path| async move {
        let _permit = semaphore.acquire().await.unwrap();
        let listing = file_ops.read_dir(&path).await;
        (path, listing)
    };

    for path in paths {
        match file_ops.is_ignored(&path).await {
            Ok(true) => {
                // TODO(cjhopman): Ignoring this matches buck1 behavior, but we'd like this to be an error.
            }
            Ok(false) => {
                if seen.insert(path.clone()) {
                    queue.push(list_dir(path));
                }
            }
            Err(e) => func(Err(
                e.context(format!("when resolving recursive spec `{}/...`", path))
            ))?,
        }
    }

    while let Some((path, listing)) = queue.next().await {
        let (buildfile_candidates, listing) = match cell_resolver
            .get(path.cell())
            .and_then(|cell_instance| anyhow::Ok((cell_instance.buildfiles(), listing?)))
        {
            Ok(r) => r,
            Err(e) => {
                func(Err(e.context(format!(
                    "when resolving recursive spec `{}/...`",
                    path
                ))))?;
                continue;
            }
        };

        if find_buildfile(buildfile_candidates, &listing).is_some() {
            func(Ok(Package::from_cell_path(&path)))?;
        }

        // The rev() call isn't necessary, it ends up causing us to slightly prefer running
        // things in order and we've encountered a specific case that benefits a lot from it
        // (due to having some huge things in an `apps/` dir).
        for entry in listing.iter().rev() {
            if entry.file_type.is_dir() {
                let child =
                    path.join_unnormalized(ForwardRelativePath::unchecked_new(&entry.file_name));
                if seen.insert(child.clone()) {
                    queue.push(list_dir(child));
                }
            }
        }
    }

    Ok(())
}

/// From a given root, this will walk the file tree and find all recursive "package
/// roots" (i.e. directories that contain build files). It will return all the
/// found roots.
pub(crate) async fn find_package_roots(
    cell_path: CellPath,
    fs: &dyn FileOps,
    cells: &CellResolver,
) -> anyhow::Result<Vec<Package>> {
    let mut results = Vec::new();
    find_package_roots_impl(fs, cells, vec![cell_path], |res| {
        results.push(res);
        Result::<_, !>::Ok(())
    })
    .await?;
    let mut results: Vec<_> = results.into_try_map(|v| v)?;
    results.sort();
    Ok(results)
}
