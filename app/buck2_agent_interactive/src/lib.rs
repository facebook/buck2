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
    Clean,
    NewIsolationDir,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
            Self::Clean => "clean",
            Self::NewIsolationDir => "new_isolation_dir",
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
            Self::Clean => {
                "No build state or outputs have been removed.\n\n\
                 `buck2 clean` stops the daemon and removes reusable build state and outputs, \
                 making subsequent builds more expensive. Buck tracks input changes automatically; \
                 retry the build or test without cleaning first. Use `buck2 clean --dry-run` \
                 to inspect the affected paths, or `buck2 clean --stale` to reclaim stale outputs.\n\n\
                 If a full clean is necessary, retry with `--agent-context advice_ack=clean` \
                 and `--agent-context reason=\"<why build state must be cleared>\"`. \
                 This guard does not require user confirmation."
            }
            Self::NewIsolationDir => {
                "No daemon has been started for this isolation directory.\n\n\
                 A new isolation directory starts a separate daemon, consumes additional memory, \
                 and loses reuse of existing build state. Prefer the default isolation directory \
                 or reuse the isolation directory already used for this work. Check both \
                 `--isolation-dir` and `BUCK_ISOLATION_DIR` when choosing the directory.\n\n\
                 If a new isolation directory is necessary, retry with \
                 `--agent-context advice_ack=new_isolation_dir` and \
                 `--agent-context reason=\"<why separate daemon state is needed>\"`. \
                 This guard does not require user confirmation."
            }
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
            Self::Acknowledged(Advice::Clean) => Some("agent_interactive.clean.ack"),
            Self::Block(Advice::Clean) => Some("agent_interactive.clean.block"),
            Self::Acknowledged(Advice::NewIsolationDir) => {
                Some("agent_interactive.new_isolation_dir.ack")
            }
            Self::Block(Advice::NewIsolationDir) => {
                Some("agent_interactive.new_isolation_dir.block")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_enabled_direct_calls_are_guarded() {
        for advice in [Advice::Killall, Advice::Clean, Advice::NewIsolationDir] {
            for (enabled, direct_call) in [(false, false), (false, true), (true, false)] {
                let invocation = Invocation {
                    enabled,
                    direct_call,
                    reason: None,
                };
                assert_eq!(advice.evaluate(&invocation, false), Decision::Inactive);
            }
        }
    }

    #[test]
    fn proceeding_requires_acknowledgement_and_reason() {
        for advice in [Advice::Killall, Advice::Clean, Advice::NewIsolationDir] {
            for (reason, expected) in [
                (None, Decision::Block(advice)),
                (Some(""), Decision::Block(advice)),
                (Some(" \n\t"), Decision::Block(advice)),
                (
                    Some("Several daemons are unresponsive"),
                    Decision::Acknowledged(advice),
                ),
            ] {
                let invocation = Invocation {
                    enabled: true,
                    direct_call: true,
                    reason,
                };
                assert_eq!(advice.evaluate(&invocation, false), Decision::Block(advice));
                assert_eq!(advice.evaluate(&invocation, true), expected);
            }
        }
    }
}
