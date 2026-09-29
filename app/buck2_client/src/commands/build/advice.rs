/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_client_ctx::agent_context::AgentContext;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_common::settings::settings::BuildIntentMode;
use buck2_error::buck2_error;
use dupe::Dupe;

const BUILD_INTENT: &str = "build_intent";

enum BuildIntentDecision<'a> {
    Acknowledged,
    Check,
    MissingMessage,
    Warn(&'a str),
    Block(&'a str),
}

impl BuildIntentDecision<'_> {
    fn tag(&self) -> &'static str {
        match self {
            Self::Acknowledged => "agent_advice.build_intent.ack",
            Self::Check => "agent_advice.build_intent.check",
            Self::MissingMessage => "agent_advice.build_intent.missing_message",
            Self::Warn(_) => "agent_advice.build_intent.warn",
            Self::Block(_) => "agent_advice.build_intent.block",
        }
    }
}

pub(super) async fn check_build_intent(
    patterns: &[String],
    ctx: &ClientCommandContext<'_>,
    events_ctx: &mut EventsCtx,
) -> buck2_error::Result<()> {
    if !ctx.agent_context().is_direct_call() || patterns.is_empty() {
        return Ok(());
    }

    let settings = &ctx
        .immediate_config
        .daemon_startup_config()?
        .buck_settings
        .agent_advice;
    let Some(decision) = decide(
        settings.build_intent_mode(),
        settings.build_intent_message(),
        ctx.agent_context(),
        patterns,
    ) else {
        return Ok(());
    };
    events_ctx
        .instant_event(
            ctx.trace_id.dupe(),
            buck2_data::TagEvent {
                tags: vec![decision.tag().to_owned()],
            },
        )
        .await?;

    match decision {
        BuildIntentDecision::Acknowledged | BuildIntentDecision::Check => Ok(()),
        BuildIntentDecision::MissingMessage => {
            events_ctx
                .instant_event(
                    ctx.trace_id.dupe(),
                    buck2_data::ConsoleWarning {
                        message: format!(
                            "Buck advice [{BUILD_INTENT}] skipped: agent_advice.build_intent_message must be non-empty when the advice is enabled."
                        ),
                    },
                )
                .await
        }
        BuildIntentDecision::Warn(message) => {
            events_ctx
                .instant_event(
                    ctx.trace_id.dupe(),
                    buck2_data::ConsoleWarning {
                        message: format!("Buck advice [{BUILD_INTENT}]\n\n{message}"),
                    },
                )
                .await
        }
        BuildIntentDecision::Block(message) => Err(buck2_error!(
            buck2_error::ErrorTag::AgentAdviceBlocked,
            "Buck advice [{BUILD_INTENT}]\nThe requested build has not started.\n\n{message}\n\nChoose the operation that matches the task. To continue this build, add `--agent-context advice_ack={BUILD_INTENT}` after `buck2`. No user confirmation is needed."
        )),
    }
}

fn decide<'a>(
    mode: BuildIntentMode,
    message: Option<&'a str>,
    agent_context: &AgentContext,
    patterns: &[String],
) -> Option<BuildIntentDecision<'a>> {
    if mode == BuildIntentMode::Off {
        return None;
    }
    if agent_context.is_advice_acknowledged(BUILD_INTENT) {
        return Some(BuildIntentDecision::Acknowledged);
    }
    // Only recognize an explicit check request; aliases and rule defaults need target loading.
    if patterns.iter().all(|pattern| pattern.ends_with("[check]")) {
        return Some(BuildIntentDecision::Check);
    }
    let Some(message) = message.filter(|message| !message.trim().is_empty()) else {
        return Some(BuildIntentDecision::MissingMessage);
    };
    match mode {
        BuildIntentMode::Off => None,
        BuildIntentMode::Warn => Some(BuildIntentDecision::Warn(message)),
        BuildIntentMode::Block => Some(BuildIntentDecision::Block(message)),
    }
}
