/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Agent-interactive policy shared by client and daemon integration points.
//! Callers supply invocation facts and apply decisions through their own event/error paths.

/// Effective options for one command, independent of CLI parsing and daemon state.
pub struct Invocation<'a> {
    pub enabled: bool,
    pub direct_call: bool,
    pub reason: Option<&'a str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Advice {
    Killall,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Decision {
    Inactive,
    Acknowledged(Advice),
    Block(Advice),
}

impl Advice {
    /// Stable identifier used for acknowledgements and telemetry.
    pub fn id(self) -> &'static str {
        match self {
            Self::Killall => "killall",
        }
    }

    /// `acknowledged` must refer to this advice ID in the current invocation.
    pub fn evaluate(self, invocation: &Invocation<'_>, acknowledged: bool) -> Decision {
        if !invocation.enabled || !invocation.direct_call {
            return Decision::Inactive;
        }
        if acknowledged
            && invocation
                .reason
                .is_some_and(|reason| !reason.trim().is_empty())
        {
            Decision::Acknowledged(self)
        } else {
            Decision::Block(self)
        }
    }

    pub fn message(self) -> &'static str {
        match self {
            Self::Killall => {
                "No processes have been killed.\n\n\
                 `buck2 killall` discards reusable daemon state and can interrupt other work. \
                 Inspect `buck2 status` first. If one daemon needs restarting, prefer `buck2 kill` \
                 in its repository with the appropriate `--isolation-dir`.\n\
                 To limit `killall`, use `--repo` and/or `--in-isolation-dir`; \
                 the global `--isolation-dir` flag does not limit which processes it kills.\n\n\
                 If killall is necessary, retry with `--agent-context advice_ack=killall` \
                 and `--agent-context reason=\"<why killall is needed>\"`. \
                 This guard does not require user confirmation."
            }
        }
    }
}

impl Decision {
    pub fn tag(&self) -> Option<&'static str> {
        match self {
            Self::Inactive => None,
            Self::Acknowledged(Advice::Killall) => Some("agent_interactive.killall.ack"),
            Self::Block(Advice::Killall) => Some("agent_interactive.killall.block"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_enabled_direct_calls_are_guarded() {
        for (enabled, direct_call) in [(false, false), (false, true), (true, false)] {
            let invocation = Invocation {
                enabled,
                direct_call,
                reason: None,
            };
            assert_eq!(
                Advice::Killall.evaluate(&invocation, false),
                Decision::Inactive
            );
        }
    }

    #[test]
    fn proceeding_requires_acknowledgement_and_reason() {
        for (reason, expected) in [
            (None, Decision::Block(Advice::Killall)),
            (Some(""), Decision::Block(Advice::Killall)),
            (Some(" \n\t"), Decision::Block(Advice::Killall)),
            (
                Some("Several daemons are unresponsive"),
                Decision::Acknowledged(Advice::Killall),
            ),
        ] {
            let invocation = Invocation {
                enabled: true,
                direct_call: true,
                reason,
            };
            assert_eq!(
                Advice::Killall.evaluate(&invocation, false),
                Decision::Block(Advice::Killall)
            );
            assert_eq!(Advice::Killall.evaluate(&invocation, true), expected);
        }
    }
}
