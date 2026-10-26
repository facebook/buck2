/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_common::file_ops::metadata::FileMetadata;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorOptionContext;
use buck2_error::internal_error;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::materialize::materializer::DeclareArtifactPayload;
use buck2_execute::materialize::materializer::WriteRequest;
use dupe::Dupe;
use itertools::Itertools;

use crate::actions::impls::cas_probe;

/// Declares the outputs of a write action. Returns their values in the order `generate`
/// produced them, and `Deferred` when none of them had to be written because the CAS already
/// held every output's content, `Simple` otherwise.
///
/// Whether the CAS is asked at all is `cas_probe`'s decision; when it is not, this is a plain
/// `declare_write`. When it is, `generate` runs twice if anything is missing from the CAS: the
/// content is hashed and dropped before the CAS is asked, since thousands of writes wait on that
/// answer at once in a cold build and their content in hand was most of the daemon's peak
/// memory (RESULTS.md, 2026-10-13); the misses, few, are generated again to be written.
pub(crate) async fn declare_writes(
    ctx: &dyn ActionExecutionCtx,
    mut generate: Box<dyn FnMut() -> buck2_error::Result<Vec<WriteRequest>> + Send + '_>,
) -> buck2_error::Result<(Vec<ArtifactValue>, ActionExecutionKind)> {
    if !cas_probe::enabled(ctx) {
        let values = ctx.materializer().declare_write(generate).await?;
        return Ok((values, ActionExecutionKind::Simple));
    }

    let cas_digest_config = ctx.digest_config().cas_digest_config();
    let outputs: Vec<(ProjectRelativePathBuf, bool, TrackedFileDigest)> = generate()?
        .into_iter()
        .map(|request| {
            let digest = TrackedFileDigest::from_content(&request.content, cas_digest_config);
            (request.path, request.is_executable, digest)
        })
        .collect();
    let digests: Vec<TrackedFileDigest> =
        outputs.iter().map(|(_, _, digest)| digest.dupe()).collect();
    let Some((info, held)) = cas_probe::probe(ctx, digests).await else {
        let values = ctx.materializer().declare_write(generate).await?;
        return Ok((values, ActionExecutionKind::Simple));
    };

    let mut values: Vec<Option<ArtifactValue>> = vec![None; outputs.len()];
    let mut in_cas = Vec::new();
    let mut missed: Vec<(usize, ProjectRelativePathBuf, TrackedFileDigest)> = Vec::new();
    for (i, ((path, is_executable, digest), held)) in outputs.into_iter().zip_eq(held).enumerate() {
        if held {
            // The check stamped the expiration onto `digest`.
            let value = ArtifactValue::file(FileMetadata {
                digest,
                is_executable,
            });
            in_cas.push(DeclareArtifactPayload {
                path,
                artifact: value.dupe(),
            });
            values[i] = Some(value);
        } else {
            missed.push((i, path, digest));
        }
    }

    let kind = if missed.is_empty() {
        ActionExecutionKind::Deferred
    } else {
        ActionExecutionKind::Simple
    };
    if !in_cas.is_empty() {
        ctx.materializer().declare_cas_many(info, in_cas).await?;
    }
    if !missed.is_empty() {
        let mut requests: Vec<Option<WriteRequest>> = generate()?.into_iter().map(Some).collect();
        let mut to_write = Vec::with_capacity(missed.len());
        for (i, path, digest) in &missed {
            let request = requests
                .get_mut(*i)
                .and_then(Option::take)
                .internal_error("a write action generated fewer outputs the second time")?;
            if request.path != *path
                || TrackedFileDigest::from_content(&request.content, cas_digest_config).data()
                    != digest.data()
            {
                return Err(internal_error!(
                    "A write action generated `{path}` differently the second time"
                ));
            }
            to_write.push(request);
        }
        let written = ctx
            .materializer()
            .declare_write(Box::new(move || Ok(to_write)))
            .await?;
        for ((i, _, _), value) in missed.into_iter().zip(written) {
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
