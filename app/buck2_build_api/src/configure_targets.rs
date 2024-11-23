/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::configuration::compatibility::IncompatiblePlatformReason;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_events::dispatch::console_message;
use buck2_node::load_patterns::load_patterns;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use dice::DiceComputations;
use dupe::Dupe;
use futures::FutureExt;
use gazebo::prelude::VecExt;
use starlark_map::small_set::SmallSet;

// Returns a tuple of compatible and incompatible targets.
fn split_compatible_incompatible(
    targets: impl IntoIterator<Item = buck2_error::Result<MaybeCompatible<ConfiguredTargetNode>>>,
) -> buck2_error::Result<(
    TargetSet<ConfiguredTargetNode>,
    SmallSet<ConfiguredTargetLabel>,
)> {
    let mut target_set = TargetSet::new();
    let mut incompatible_targets = SmallSet::new();

    for res in targets {
        match res? {
            MaybeCompatible::Incompatible(reason) => {
                incompatible_targets.insert(reason.target.dupe());
            }
            MaybeCompatible::Compatible(target) => {
                target_set.insert(target);
            }
        }
    }
    Ok((target_set, incompatible_targets))
}

pub async fn get_maybe_compatible_targets<'a>(
    ctx: &'a mut DiceComputations<'_>,
    loaded_targets: impl IntoIterator<Item = (PackageLabel, buck2_error::Result<Vec<TargetNode>>)>,
    global_cfg_options: &GlobalCfgOptions,
    keep_going: bool,
) -> buck2_error::Result<
    impl Iterator<Item = buck2_error::Result<MaybeCompatible<ConfiguredTargetNode>>>,
> {
    let mut by_package_fns: Vec<_> = Vec::new();

    for (_package, result) in loaded_targets {
        match result {
            Ok(targets) => {
                by_package_fns.extend({
                    let target_fns: Vec<_> = targets.into_map(|target| {
                        DiceComputations::declare_closure(|ctx| {
                            async move {
                                let target = ctx
                                    .get_configured_target(target.label(), global_cfg_options)
                                    .await?;
                                buck2_error::Ok(ctx.get_configured_target_node(&target).await?)
                            }
                            .boxed()
                        })
                    });

                    target_fns
                });
            }
            Err(e) => {
                // TODO(@wendyy) - log the error
                if !keep_going {
                    return Err(e.into());
                }
            }
        }
    }

    Ok(futures::future::join_all(ctx.compute_many(by_package_fns))
        .await
        .into_iter())
}

/// Converts target nodes to a set of compatible configured target nodes.
pub async fn get_compatible_targets(
    ctx: &mut DiceComputations<'_>,
    loaded_targets: impl IntoIterator<Item = (PackageLabel, buck2_error::Result<Vec<TargetNode>>)>,
    global_cfg_options: &GlobalCfgOptions,
) -> buck2_error::Result<TargetSet<ConfiguredTargetNode>> {
    let maybe_compatible_targets =
        get_maybe_compatible_targets(ctx, loaded_targets, global_cfg_options, false).await?;

    let (compatible_targets, incompatible_targets) =
        split_compatible_incompatible(maybe_compatible_targets)?;

    if !incompatible_targets.is_empty() {
        console_message(IncompatiblePlatformReason::skipping_message_for_multiple(
            incompatible_targets.iter(),
        ));
    }

    Ok(compatible_targets)
}

pub async fn load_compatible_patterns(
    ctx: &mut DiceComputations<'_>,
    parsed_patterns: Vec<ParsedPattern<TargetPatternExtra>>,
    global_cfg_options: &GlobalCfgOptions,
    skip_missing_targets: MissingTargetBehavior,
) -> buck2_error::Result<TargetSet<ConfiguredTargetNode>> {
    let loaded_patterns = load_patterns(ctx, parsed_patterns, skip_missing_targets).await?;
    get_compatible_targets(
        ctx,
        loaded_patterns.iter_loaded_targets_by_package(),
        global_cfg_options,
    )
    .await
}
