/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Content an action produces in the daemon, a write or a copy from a source file, that the CAS
//! already holds is declared as CAS content instead of being produced: the probe behind the
//! `write_cas_probe` knob.

use std::sync::Arc;

use buck2_build_api::actions::ActionExecutionCtx;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_directory::directory::walk::unordered_entry_walk;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use buck2_execute::materialize::materializer::DeclareArtifactPayload;
use buck2_execute::re::presence::NegativeCache;
use dupe::Dupe;

/// Whether the CAS is asked before content is produced: the knob's decision, and never when the
/// invocation asks the CAS nothing (`ActionExecutionCtx::may_probe_cas`).
pub(crate) fn enabled(ctx: &dyn ActionExecutionCtx) -> bool {
    ctx.run_action_knobs().write_cas_probe && ctx.may_probe_cas()
}

/// Which of `digests` the CAS holds, one answer per digest, with the `CasDownloadInfo` to
/// declare the hits under; a hit's digest carries the expiration the check found. `None` when
/// the CAS could not answer, which is reported once and is every digest missing to the caller.
pub(crate) async fn probe(
    ctx: &dyn ActionExecutionCtx,
    digests: Vec<TrackedFileDigest>,
) -> Option<(Arc<CasDownloadInfo>, Vec<bool>)> {
    let use_case = ctx.invocation_re_use_case();
    let info = Arc::new(CasDownloadInfo::new_probed(use_case));
    // A stale miss only costs producing what would otherwise have been skipped.
    let presence = match ctx
        .re_client()
        .with_use_case(use_case)
        .check_presence(digests, NegativeCache::Allowed, &info)
        .await
    {
        Ok(presence) => presence,
        Err(e) => {
            // Producing the content is the right fallback, but a CAS that cannot answer would
            // silently turn every probe into a miss, which should be visible.
            buck2_core::soft_error_or_panic!(
                "write_cas_probe_failed",
                e.context("CAS lookup for content an action produces failed; producing it instead")
            );
            return None;
        }
    };
    let in_cas = presence
        .into_iter()
        .map(|presence| presence.is_some())
        .collect();
    Some((info, in_cas))
}

/// Declares `value` at `path` as CAS content if the CAS holds every file in it, and says whether
/// it did; when it did not, the content is the caller's to produce.
pub(crate) async fn declare_if_in_cas(
    ctx: &dyn ActionExecutionCtx,
    path: ProjectRelativePathBuf,
    value: &ArtifactValue,
) -> buck2_error::Result<bool> {
    if !enabled(ctx) {
        return Ok(false);
    }
    let digests = file_digests(value);
    if digests.is_empty() {
        // All structure, which is the caller's to make either way.
        return Ok(false);
    }
    let Some((info, in_cas)) = probe(ctx, digests).await else {
        return Ok(false);
    };
    if !in_cas.iter().all(|&in_cas| in_cas) {
        return Ok(false);
    }
    ctx.materializer()
        .declare_cas_many(
            info,
            vec![DeclareArtifactPayload {
                path,
                artifact: value.dupe(),
            }],
        )
        .await?;
    Ok(true)
}

/// Asks the CAS about `digests` so that the ones it holds carry an expiration, and says whether it
/// held every one of them. Nothing is declared: the caller declares as it would have, and content
/// whose digest carries an expiration is left out of the stash when the declaration offers it, so
/// a probe here is what keeps the CAS's share of a copied directory from being copied into the
/// store. Asks nothing when the probe is disabled or there is nothing to ask, and answers `false`
/// then, as for a CAS that could not answer.
pub(crate) async fn stamp_if_in_cas(
    ctx: &dyn ActionExecutionCtx,
    digests: Vec<TrackedFileDigest>,
) -> bool {
    if digests.is_empty() || !enabled(ctx) {
        return false;
    }
    match probe(ctx, digests).await {
        Some((_, in_cas)) => in_cas.iter().all(|&in_cas| in_cas),
        None => false,
    }
}

fn file_digests(value: &ArtifactValue) -> Vec<TrackedFileDigest> {
    file_digests_of(value.entry())
}

/// The digests of the files in `entry`, in walk order.
pub(crate) fn file_digests_of(
    entry: &ActionDirectoryEntry<ActionSharedDirectory>,
) -> Vec<TrackedFileDigest> {
    let mut digests = Vec::new();
    let mut walk = unordered_entry_walk(entry.as_ref().map_dir(Directory::as_ref));
    while let Some((_, entry)) = walk.next() {
        if let DirectoryEntry::Leaf(ActionDirectoryMember::File(file)) = entry {
            digests.push(file.digest.dupe());
        }
    }
    digests
}
