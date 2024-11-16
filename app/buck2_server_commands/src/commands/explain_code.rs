/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use core::iter::Iterator;
use std::collections::HashMap;

use anyhow::Context;
use buck2_cli_proto::new_generic::ExplainRequest;
use buck2_core::pattern::pattern_type::ConfiguredTargetPatternExtra;
use buck2_data::action_key;
use buck2_event_log::read::EventLogPathBuf;
use buck2_event_log::stream_value::StreamValue;
use buck2_event_observer::display::display_anon_target;
use buck2_event_observer::display::display_bxl_key;
use buck2_event_observer::display::display_configured_target_label;
use buck2_event_observer::display::TargetDisplayOptions;
use buck2_event_observer::what_ran::CommandReproducer;
use buck2_event_observer::what_ran::WhatRanOptions;
use buck2_event_observer::what_ran::WhatRanRelevantAction;
use buck2_events::span::SpanId;
use buck2_explain::ActionEntryData;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_query::query::syntax::simple::eval::label_indexed::LabelIndexedSet;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::pattern_parse_and_resolve::parse_and_resolve_patterns_to_targets_from_cli_args;
use buck2_server_ctx::target_resolution_config::TargetResolutionConfig;
use dice::DiceTransaction;
use dupe::Dupe;
use dupe::IterDupedExt;
use futures::TryStreamExt;

#[allow(clippy::vec_box)]
#[cfg(fbcode_build)]
struct ActionEntry {
    /// Known to be a WhatRanRelevantAction.
    event: Box<buck2_data::BuckEvent>,

    /// Known to be a CommandReproducer.
    reproducers: Vec<Box<buck2_data::BuckEvent>>,
}

#[cfg(fbcode_build)]
impl ActionEntry {
    fn format_action(
        &self,
        data: &buck2_data::span_end_event::Data,
        options: &WhatRanOptions,
    ) -> anyhow::Result<Option<(String, ActionEntryData)>> {
        let action = WhatRanRelevantAction::from_buck_data(
            self.event.data.as_ref().context("Checked above")?,
        );

        let failed = match data {
            buck2_data::span_end_event::Data::ActionExecution(action_exec) => Some(action_exec),
            _ => None,
        }.expect("Should always be an ActionExecution end event because span ID must match ActionExecution start event.").failed;

        let (target, mut entry) = match action {
            Some(WhatRanRelevantAction::ActionExecution(act)) => {
                let category = act.name.as_ref().map(|n| n.category.clone());
                let owner = match act.key.as_ref() {
                    Some(key) => key.owner.as_ref(),
                    None => return Ok(None),
                };

                let opts = TargetDisplayOptions::for_log();
                let target = match owner {
                    Some(o) => match o {
                        action_key::Owner::TargetLabel(target_label)
                        | action_key::Owner::TestTargetLabel(target_label)
                        | action_key::Owner::LocalResourceSetup(target_label) => {
                            display_configured_target_label(&target_label, opts)
                        }
                        action_key::Owner::BxlKey(bxl_key) => display_bxl_key(&bxl_key),
                        action_key::Owner::AnonTarget(anon_target) => {
                            display_anon_target(&anon_target)
                        }
                    }?,
                    None => return Ok(None),
                };

                (
                    target,
                    ActionEntryData {
                        category,
                        failed,
                        repros: vec![],
                    },
                )
            }
            _ => return Ok(None),
        };

        for repro in self.reproducers.iter() {
            let reproducer = CommandReproducer::from_buck_data(
                repro.data.as_ref().expect("Checked above"),
                options,
            )
            .map(|r| r.as_human_readable());
            if let Some(repro) = reproducer {
                entry.repros.push(repro.to_string());
            }
        }

        Ok(Some((target, entry)))
    }
}

pub(crate) async fn explain(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    req: &ExplainRequest,
) -> anyhow::Result<()> {
    let build_log = EventLogPathBuf::infer(req.log_path.clone())?;
    let (_, mut events) = build_log.unpack_stream().await?;

    let options = WhatRanOptions {
        skip_cache_hits: true,
        emit_cache_queries: false,
        ..Default::default()
    };
    let mut known_actions: HashMap<SpanId, ActionEntry> = Default::default();

    let mut executed_actions = vec![];

    while let Some(event) = events.try_next().await? {
        match event {
            StreamValue::Event(event) => {
                // TODO iguridi: deduplicate this from whatran code
                if let Some(data) = &event.data {
                    if WhatRanRelevantAction::from_buck_data(data).is_some() {
                        known_actions.insert(
                            SpanId::from_u64(event.span_id)?,
                            ActionEntry {
                                event: event.clone(),
                                reproducers: Default::default(),
                            },
                        );
                    }
                    if CommandReproducer::from_buck_data(data, &options).is_some() {
                        if let Some(parent_id) = SpanId::from_u64_opt(event.parent_id) {
                            if let Some(entry) = known_actions.get_mut(&parent_id) {
                                entry.reproducers.push(event.clone());
                            }
                        }
                    }

                    match data {
                        buck2_data::buck_event::Data::SpanEnd(span) => {
                            if let Some(entry) =
                                known_actions.remove(&SpanId::from_u64(event.span_id)?)
                            {
                                if let Some(data) = &span.data {
                                    if let Some(entry) = entry.format_action(data, &options)? {
                                        executed_actions.push(entry);
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    let configured_target = {
        // TODO iguridi: this is hacky
        let target_pattern = parse_and_resolve_patterns_to_targets_from_cli_args::<
            ConfiguredTargetPatternExtra,
        >(&mut ctx, &[req.target.clone()], server_ctx.working_dir())
        .await?;

        let target_label = match target_pattern.as_slice() {
            [p] => &p.target_label,
            _ => {
                return Err(anyhow::anyhow!(
                    "Expected exactly one target, got {}",
                    target_pattern.len()
                ));
            }
        };

        let target_resolution_config = TargetResolutionConfig::from_args(
            &mut ctx,
            &req.target_cfg,
            server_ctx,
            &req.target_universe,
        )
        .await?;

        let configured_targets = target_resolution_config
            .get_configured_target(&mut ctx, target_label)
            .await?;
        if configured_targets.len() != 1 {
            return Err(anyhow::anyhow!(
                "Expected exactly one target, got {}",
                configured_targets.len()
            ));
        }
        ctx.get_configured_target_node(&configured_targets[0])
            .await?
            .require_compatible()? // TODO iguridi: not sure about this, make things simpler for now
    };

    let all_deps = {
        let mut stack = vec![configured_target];
        let mut visited = LabelIndexedSet::new();
        while let Some(node) = stack.pop() {
            if visited.insert(node.dupe()) {
                stack.extend(node.deps().duped());
            }
        }
        visited.into_iter().collect::<Vec<ConfiguredTargetNode>>()
    };

    buck2_explain::main(
        all_deps,
        executed_actions,
        req.output.as_ref(),
        req.fbs_dump.as_ref(),
        req.manifold_path.as_deref(),
    )
    .await?;

    Ok(())
}
