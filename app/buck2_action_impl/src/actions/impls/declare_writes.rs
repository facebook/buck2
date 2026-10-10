/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_common::file_ops::metadata::FileMetadata;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use buck2_execute::materialize::materializer::DeclareArtifactPayload;
use buck2_execute::materialize::materializer::WriteRequest;
use buck2_execute::re::presence::NegativeCache;
use dupe::Dupe;
use itertools::Itertools;

/// Declares the outputs of a write action. Returns their values in the order `generate`
/// produced them, and `Deferred` when none of them had to be written because the CAS already
/// held every output's content, `Simple` otherwise.
///
/// Whether the CAS is asked at all is the `write_cas_probe` knob's decision; without it, or
/// without a CAS to ask, this is a plain `declare_write`.
pub(crate) async fn declare_writes(
    ctx: &dyn ActionExecutionCtx,
    generate: Box<dyn FnOnce() -> buck2_error::Result<Vec<WriteRequest>> + Send + '_>,
) -> buck2_error::Result<(Vec<ArtifactValue>, ActionExecutionKind)> {
    if !ctx.run_action_knobs().write_cas_probe || !ctx.cas_configured() {
        let values = ctx.materializer().declare_write(generate).await?;
        return Ok((values, ActionExecutionKind::Simple));
    }
    let use_case = ctx.invocation_re_use_case();

    let requests = generate()?;
    let cas_digest_config = ctx.digest_config().cas_digest_config();
    let digests: Vec<TrackedFileDigest> = requests
        .iter()
        .map(|request| TrackedFileDigest::from_content(&request.content, cas_digest_config))
        .collect();

    let info = Arc::new(CasDownloadInfo::new_probed(use_case));
    // A stale miss only costs the write that would otherwise have been skipped.
    let presence = match ctx
        .re_client()
        .with_use_case(use_case)
        .check_presence(digests.clone(), NegativeCache::Allowed, &info)
        .await
    {
        Ok(presence) => presence,
        Err(e) => {
            // Writing is the right fallback for this action, but a CAS that cannot answer would
            // silently turn every write back into a deferred write, which should be visible.
            let _ignored = buck2_core::soft_error!(
                "write_cas_probe_failed",
                e.context("CAS lookup for write action content failed; writing instead"),
                quiet: true
            );
            vec![None; requests.len()]
        }
    };

    let mut values: Vec<Option<ArtifactValue>> = vec![None; requests.len()];
    let mut in_cas = Vec::new();
    let mut to_write = Vec::new();
    for (i, ((request, digest), presence)) in requests
        .into_iter()
        .zip_eq(digests)
        .zip_eq(presence)
        .enumerate()
    {
        match presence {
            Some(_) => {
                // The check stamped the expiration onto `digest`.
                let value = ArtifactValue::file(FileMetadata {
                    digest,
                    is_executable: request.is_executable,
                });
                in_cas.push(DeclareArtifactPayload {
                    path: request.path,
                    artifact: value.dupe(),
                });
                values[i] = Some(value);
            }
            _ => to_write.push((i, request)),
        }
    }

    let kind = if to_write.is_empty() {
        ActionExecutionKind::Deferred
    } else {
        ActionExecutionKind::Simple
    };
    if !in_cas.is_empty() {
        ctx.materializer().declare_cas_many(info, in_cas).await?;
    }
    if !to_write.is_empty() {
        let (indices, requests): (Vec<usize>, Vec<WriteRequest>) = to_write.into_iter().unzip();
        let written = ctx
            .materializer()
            .declare_write(Box::new(move || Ok(requests)))
            .await?;
        for (i, value) in indices.into_iter().zip(written) {
            values[i] = Some(value);
        }
    }

    Ok((
        values
            .into_iter()
            .map(|value| value.expect("every request was declared one way or the other"))
            .collect(),
        kind,
    ))
}
