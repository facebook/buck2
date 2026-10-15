/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use crate::agent_context::AgentContext;

/// Agent inputs and an independent observation for one client invocation.
/// The default has no metadata or inference and does not inspect processes.
#[derive(Clone, Debug, Default)]
pub struct AgentInvocationInfo {
    agent_context: AgentContext,
    /// Best-effort client-ancestry inference, independent of `agent_context`.
    /// `Some(true)` is a heuristic match; `Some(false)` is a non-match.
    /// `None` means inference has not run or inspection was inconclusive.
    inferred_direct_call: Option<bool>,
}

impl AgentInvocationInfo {
    /// Record inference in the client: a shared daemon's ancestry says nothing
    /// about the caller of an individual command.
    pub fn from_client(agent_context: AgentContext) -> Self {
        Self::with_inferred_direct_call(agent_context, || {
            #[cfg(all(fbcode_build, target_os = "linux"))]
            {
                crate::agent_detection::infer_direct_agent_call()
            }
            #[cfg(not(all(fbcode_build, target_os = "linux")))]
            {
                None
            }
        })
    }

    fn with_inferred_direct_call(
        agent_context: AgentContext,
        detect: impl FnOnce() -> Option<bool>,
    ) -> Self {
        Self {
            agent_context,
            inferred_direct_call: detect(),
        }
    }

    pub(crate) fn inferred_direct_call(&self) -> Option<bool> {
        self.inferred_direct_call
    }

    pub(crate) fn agent_context(&self) -> &AgentContext {
        &self.agent_context
    }
}

#[cfg(test)]
mod tests {
    use buck2_data::AgentContextEntry as AgentContextKeys;

    use super::*;
    use crate::agent_context::parse_agent_context;

    #[test]
    fn inference_is_independent_of_explicit_markers() {
        for inferred in [Some(true), Some(false), None] {
            for explicit in [None, Some("true"), Some("false"), Some("invalid"), Some("")] {
                let entries = explicit
                    .map(|value| parse_agent_context(&format!("direct_call={value}")).unwrap())
                    .into_iter()
                    .collect();
                let info = AgentInvocationInfo::with_inferred_direct_call(
                    AgentContext::new(entries),
                    || inferred,
                );
                assert_eq!(
                    info.agent_context().is_direct_call(),
                    explicit == Some("true")
                );
                let entries = info.agent_context().to_proto();
                assert_eq!(
                    entries
                        .iter()
                        .find(|e| e.key == AgentContextKeys::KEY_DIRECT_CALL)
                        .map(|e| e.value.as_str()),
                    explicit,
                );
                assert_eq!(info.inferred_direct_call(), inferred);
                assert_eq!(entries.len(), usize::from(explicit.is_some()));
            }
        }
    }

    #[test]
    fn inference_cannot_be_overridden_by_cli_entries() {
        for inferred in [Some(true), Some(false), None] {
            let entries = ["inferred_direct_call=true", "inferred_direct_call=false"]
                .map(|value| parse_agent_context(value).unwrap())
                .to_vec();
            let info =
                AgentInvocationInfo::with_inferred_direct_call(AgentContext::new(entries), || {
                    inferred
                });
            assert!(!info.agent_context().is_direct_call());
            assert_eq!(info.inferred_direct_call(), inferred);
            assert_eq!(info.agent_context().to_proto().len(), 2);
        }
    }
}
