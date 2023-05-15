/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::io::Write;

use anyhow::Context;
use buck2_build_api::audit_dep_files::AUDIT_DEP_FILES;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::category::Category;
use buck2_core::directory::Directory;
use buck2_core::directory::DirectoryIterator;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::materialize::materializer::HasMaterializer;
use dice::DiceTransaction;

use crate::actions::impls::run::dep_files::get_dep_files;
use crate::actions::impls::run::dep_files::DepFilesKey;
use crate::actions::impls::run::dep_files::StoredFingerprints;

pub(crate) fn init_audit_dep_files() {
    AUDIT_DEP_FILES.init(|ctx, label, category, identifier, stdout| {
        Box::pin(audit_dep_files(ctx, label, category, identifier, stdout))
    });
}

async fn audit_dep_files(
    ctx: &DiceTransaction,
    label: ConfiguredTargetLabel,
    category: Category,
    identifier: Option<String>,
    stdout: &mut (dyn Write + Send),
) -> anyhow::Result<()> {
    let key = DepFilesKey::new(BaseDeferredKey::TargetLabel(label), category, identifier);

    let state = get_dep_files(&key).context("Failed to find dep files")?;

    let dep_files = state
        .read_dep_files(
            &ctx.get_artifact_fs().await?,
            ctx.per_transaction_data().get_materializer().as_ref(),
        )
        .await
        .context("Failed to read dep files")?
        .context("Dep fils have expired")?;

    let fingerprints = state.locked_compute_fingerprints(
        Cow::Owned(dep_files),
        true,
        ctx.global_data().get_digest_config(),
    );

    let dirs = match &*fingerprints {
        StoredFingerprints::Digests(..) => {
            // This is bit awkward but this only for testing right now so that's OK
            return Err(anyhow::anyhow!("Fingerprints were stored as digests!"));
        }
        StoredFingerprints::Dirs(dirs) => dirs,
    };

    for (path, ..) in dirs
        .untagged
        .ordered_walk()
        .with_paths()
        .filter_map(|(p, e)| Some((p, e.into_leaf()?)))
    {
        writeln!(stdout, "untagged\t{}", path)?;
    }

    for (tag, dir) in dirs.tagged.iter() {
        for (path, ..) in dir
            .ordered_walk()
            .with_paths()
            .filter_map(|(p, e)| Some((p, e.into_leaf()?)))
        {
            writeln!(stdout, "{}\t{}", tag, path)?;
        }
    }

    Ok(())
}
