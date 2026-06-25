/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::LazyLock;

use buck2_core::buck2_env;
use regex::Regex;

/// A key / value entry from --agent-context. Used to track agent intent,
/// retry state, and prior errors for SSR (Second Solve Rate) analysis.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AgentContextEntry {
    pub key: String,
    pub value: String,
}

impl AgentContextEntry {
    pub fn to_proto(&self) -> buck2_data::AgentContextEntry {
        buck2_data::AgentContextEntry {
            key: self.key.clone(),
            value: self.value.clone(),
        }
    }

    /// Read `CODING_AGENT_METADATA` env var and produce agent context entries.
    /// This env var is set exclusively by AI agent launchers (3PAI for
    /// Claude Code/Gemini/Codex, Devmate extension) and provides an
    /// unambiguous signal that an AI agent is the originator.
    pub fn from_env() -> buck2_error::Result<Vec<Self>> {
        let Some(meta) = buck2_env!("CODING_AGENT_METADATA")? else {
            return Ok(vec![]);
        };
        Ok(parse_agent_env_metadata(&meta))
    }
}

/// Parses the env var value (comma-separated key=value pairs) and returns
/// `AgentContextEntry` entries for AI agent identification.
fn parse_agent_env_metadata(meta: &str) -> Vec<AgentContextEntry> {
    let find = |key: &str| {
        let prefix = format!("{}=", key);
        meta.split(',')
            .find_map(|kv| kv.strip_prefix(&prefix))
            .map(|v| v.to_owned())
    };
    let mut result = vec![AgentContextEntry {
        key: buck2_data::AgentContextEntry::KEY_ID.to_owned(),
        value: find(buck2_data::AgentContextEntry::KEY_ID)
            .unwrap_or_else(|| "unknown_agent".to_owned()),
    }];
    if let Some(inv_id) = find(buck2_data::AgentContextEntry::KEY_INVOCATION_ID) {
        result.push(AgentContextEntry {
            key: buck2_data::AgentContextEntry::KEY_INVOCATION_ID.to_owned(),
            value: inv_id,
        });
    }
    result
}

/// Parse a single `key=value` agent context entry.
/// Used as a clap `value_parser`.
pub fn parse_agent_context(value: &str) -> buck2_error::Result<AgentContextEntry> {
    const REGEX_TEXT: &str = "^[a-z][a-z0-9]*(_[a-z][a-z0-9]*)*$";
    static REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(REGEX_TEXT).unwrap());

    let (key, val) = value
        .split_once('=')
        .ok_or_else(|| AgentContextError::InvalidFormat(value.to_owned()))?;

    if !REGEX.is_match(key) {
        return Err(AgentContextError::InvalidKey(key.to_owned()).into());
    }

    Ok(AgentContextEntry {
        key: key.to_owned(),
        value: val.to_owned(),
    })
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub enum AgentContextError {
    #[error("Invalid agent-context format: `{0}`. Each entry must be a `key=value` pair.")]
    InvalidFormat(String),

    #[error("Invalid agent-context key: `{0}`. Keys must be snake_case identifiers.")]
    InvalidKey(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_single_entry() {
        assert_eq!(
            parse_agent_context("intent=build").unwrap(),
            AgentContextEntry {
                key: "intent".to_owned(),
                value: "build".to_owned(),
            }
        );
    }

    #[test]
    fn test_parse_invalid_no_equals() {
        assert!(parse_agent_context("intent").is_err());
    }

    #[test]
    fn test_parse_invalid_key() {
        assert!(parse_agent_context("Invalid=build").is_err());
        assert!(parse_agent_context("123=build").is_err());
    }

    #[test]
    fn test_parse_value_with_special_chars() {
        let entry = parse_agent_context("prior_error=kotlin_unresolved_reference").unwrap();
        assert_eq!(entry.value, "kotlin_unresolved_reference");
    }

    #[test]
    fn test_env_metadata_claude_code() {
        let meta = "id=claude_code,invocation_id=claude_code_invocation_c9e892fa-148a-4494-8466-c1f44f1647d3";
        let result = parse_agent_env_metadata(meta);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].key, buck2_data::AgentContextEntry::KEY_ID);
        assert_eq!(result[0].value, "claude_code");
        assert_eq!(
            result[1].key,
            buck2_data::AgentContextEntry::KEY_INVOCATION_ID
        );
        assert_eq!(
            result[1].value,
            "claude_code_invocation_c9e892fa-148a-4494-8466-c1f44f1647d3"
        );
    }

    #[test]
    fn test_env_metadata_devmate() {
        let meta = "id=devmate_vscode,invocation_id=agent--38920caa-4558-45dd-bc90-b6abff501f8a,surface=VS_CODE,session_id=agent--38920caa-4558-45dd-bc90-b6abff501f8a";
        let result = parse_agent_env_metadata(meta);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].value, "devmate_vscode");
        assert_eq!(
            result[1].value,
            "agent--38920caa-4558-45dd-bc90-b6abff501f8a"
        );
    }

    #[test]
    fn test_env_metadata_codex() {
        let meta = "id=codex,invocation_id=codex_invocation_a4829951-9294-4861-a7d3-10e922fc78c0";
        let result = parse_agent_env_metadata(meta);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].value, "codex");
    }

    #[test]
    fn test_env_metadata_no_invocation_id() {
        let meta = "id=some_agent";
        let result = parse_agent_env_metadata(meta);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].value, "some_agent");
    }

    #[test]
    fn test_env_metadata_no_id() {
        let meta = "invocation_id=some_id";
        let result = parse_agent_env_metadata(meta);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].value, "unknown_agent");
        assert_eq!(result[1].value, "some_id");
    }

    #[test]
    fn test_env_metadata_empty() {
        let result = parse_agent_env_metadata("");
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].value, "unknown_agent");
    }
}
