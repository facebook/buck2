/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Server-side implementation of `buck2 targets` command
//! without `--streaming` or `--resolve-alias` arguments.

use std::io::Write;
use std::path::Path;

use buck2_build_api::calculation::load_patterns;
use buck2_build_api::calculation::MissingTargetBehavior;
use buck2_build_api::nodes::lookup::ConfiguredTargetNodeLookup;
use buck2_build_api::nodes::lookup::TargetNodeLookup;
use buck2_cli_proto::targets_request;
use buck2_cli_proto::targets_request::TargetHashFileMode;
use buck2_cli_proto::targets_request::TargetHashGraphType;
use buck2_cli_proto::TargetsResponse;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_core::target::label::TargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use dice::DiceTransaction;
use dupe::Dupe;
use dupe::OptionDupedExt;

use crate::commands::targets::fmt::Stats;
use crate::commands::targets::fmt::TargetFormatter;
use crate::commands::targets::fmt::TargetInfo;
use crate::commands::targets::mk_error;
use crate::target_hash::TargetHashes;
use crate::target_hash::TargetHashesFileMode;

pub(crate) struct TargetHashOptions {
    file_mode: TargetHashesFileMode,
    fast_hash: bool,
    graph_type: TargetHashGraphType,
    recursive: bool,
}

impl TargetHashOptions {
    pub(crate) fn new(
        request: &targets_request::Other,
        cell_resolver: &CellResolver,
        fs: &ProjectRoot,
    ) -> anyhow::Result<Self> {
        let file_mode = TargetHashFileMode::from_i32(request.target_hash_file_mode)
            .expect("buck cli should send valid target hash file mode");
        let file_mode = match file_mode {
            TargetHashFileMode::PathsOnly => {
                let modified_paths = request
                    .target_hash_modified_paths
                    .iter()
                    .map(|path| {
                        let path = AbsPath::new(Path::new(&path))?;
                        cell_resolver.get_cell_path_from_abs_path(path, fs)
                    })
                    .collect::<anyhow::Result<_>>()?;
                TargetHashesFileMode::PathsOnly(modified_paths)
            }
            TargetHashFileMode::PathsAndContents => TargetHashesFileMode::PathsAndContents,
            TargetHashFileMode::NoFiles => TargetHashesFileMode::None,
        };

        Ok(Self {
            file_mode,
            fast_hash: request.target_hash_use_fast_hash,
            graph_type: TargetHashGraphType::from_i32(request.target_hash_graph_type)
                .expect("buck cli should send valid target hash graph type"),
            recursive: request.target_hash_recursive,
        })
    }
}

pub(crate) async fn targets_batch(
    server_ctx: &dyn ServerCommandContextTrait,
    dice: DiceTransaction,
    formatter: &dyn TargetFormatter,
    parsed_patterns: Vec<ParsedPattern<TargetPatternExtra>>,
    target_platform: Option<TargetLabel>,
    hash_options: TargetHashOptions,
    keep_going: bool,
) -> anyhow::Result<TargetsResponse> {
    let results = load_patterns(&dice, parsed_patterns, MissingTargetBehavior::Fail).await?;

    let target_hashes = match hash_options.graph_type {
        TargetHashGraphType::Configured => Some(
            TargetHashes::compute::<ConfiguredTargetNode, _>(
                dice.dupe(),
                ConfiguredTargetNodeLookup(&dice),
                results.iter_loaded_targets_by_package().collect(),
                target_platform,
                hash_options.file_mode,
                hash_options.fast_hash,
                hash_options.recursive,
            )
            .await?,
        ),
        TargetHashGraphType::Unconfigured => Some(
            TargetHashes::compute::<TargetNode, _>(
                dice.dupe(),
                TargetNodeLookup(&dice),
                results.iter_loaded_targets_by_package().collect(),
                target_platform,
                hash_options.file_mode,
                hash_options.fast_hash,
                hash_options.recursive,
            )
            .await?,
        ),
        _ => None,
    };

    let mut buffer = String::new();
    formatter.begin(&mut buffer);
    let mut stats = Stats::default();
    let mut needs_separator = false;
    for (package, result) in results.iter() {
        match result {
            Ok(res) => {
                stats.success += 1;
                for (_, node) in res.iter() {
                    stats.targets += 1;
                    let target_hash = target_hashes
                        .as_ref()
                        .and_then(|hashes| hashes.get(node.label()))
                        .duped()
                        .transpose()?;
                    if needs_separator {
                        formatter.separator(&mut buffer);
                    }
                    needs_separator = true;
                    formatter.target(TargetInfo { node, target_hash }, &mut buffer)
                }
            }
            Err(e) => {
                stats.errors += 1;
                let mut stderr = String::new();

                if needs_separator {
                    formatter.separator(&mut buffer);
                }
                needs_separator = true;
                formatter.package_error(package.dupe(), e.inner(), &mut buffer, &mut stderr);

                server_ctx.stderr()?.write_all(stderr.as_bytes())?;

                if !keep_going {
                    break;
                }
            }
        }
    }
    formatter.end(&stats, &mut buffer);
    if !keep_going && stats.errors != 0 {
        Err(mk_error(stats.errors))
    } else {
        Ok(TargetsResponse {
            error_count: stats.errors,
            serialized_targets_output: buffer,
        })
    }
}
