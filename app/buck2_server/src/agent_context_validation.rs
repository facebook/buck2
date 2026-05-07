/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Validation of `--agent-context` entries against a buckconfig-defined schema.
//!
//! Schema is defined in buckconfig as:
//! ```ini
//! [agent_context]
//! enforced_clients = devmate|claude_code|agentic_runtime
//!
//! [agent_context#intent]
//! required = false
//! values = build|test|query|fix|investigate
//! description = The purpose of this buck2 invocation
//! ```

use std::collections::BTreeMap;

use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_data::AgentContextEntry;

/// Schema for a single agent context field.
struct FieldSchema {
    /// Whether this field must be provided by enforced clients.
    required: bool,
    /// Allowed values. If empty, any value is accepted (freeform).
    allowed_values: Vec<String>,
    /// Human-readable description, used in error messages.
    description: String,
}

/// Schema parsed from buckconfig `[agent_context]` + `[agent_context#*]` sections.
pub(crate) struct AgentContextSchema {
    /// Client IDs that are subject to strict validation.
    enforced_clients: Vec<String>,
    /// Per-field schema definitions.
    fields: BTreeMap<String, FieldSchema>,
}

impl AgentContextSchema {
    /// Parse the schema from buckconfig.
    pub(crate) fn from_config(config: &LegacyBuckConfig) -> Self {
        let enforced_clients = config
            .get(BuckconfigKeyRef {
                section: "agent_context",
                property: "enforced_clients",
            })
            .map(|v| v.split('|').map(|s| s.trim().to_owned()).collect())
            .unwrap_or_default();

        let mut fields = BTreeMap::new();

        // Iterate all sections to find `agent_context#*` sub-sections.
        for (section_name, section) in config.all_sections() {
            if let Some(field_name) = section_name.strip_prefix("agent_context#") {
                let allowed_values = section
                    .get("values")
                    .map(|v| v.as_str().split('|').map(|s| s.trim().to_owned()).collect())
                    .unwrap_or_default();

                let required = section
                    .get("required")
                    .is_some_and(|v| v.as_str() == "true");

                let description = section
                    .get("description")
                    .map(|v| v.as_str().to_owned())
                    .unwrap_or_default();

                fields.insert(
                    field_name.to_owned(),
                    FieldSchema {
                        required,
                        allowed_values,
                        description,
                    },
                );
            }
        }

        AgentContextSchema {
            enforced_clients,
            fields,
        }
    }

    /// Returns true if this client ID is subject to strict validation.
    fn is_enforced(&self, client_id: &str) -> bool {
        self.enforced_clients.iter().any(|c| c == client_id)
    }

    /// Returns true if the schema has any field definitions.
    fn has_schema(&self) -> bool {
        !self.fields.is_empty()
    }
}

/// Validate agent context entries against the schema.
///
/// Returns `Ok(())` if validation passes or the client is not enforced.
/// Returns `Err` with a descriptive error if validation fails for an enforced client.
pub(crate) fn validate_agent_context(
    schema: &AgentContextSchema,
    client_id: Option<&str>,
    entries: &[buck2_data::AgentContextEntry],
) -> buck2_error::Result<()> {
    // If no entries provided or no schema defined, nothing to validate.
    if entries.is_empty() || !schema.has_schema() {
        return Ok(());
    }

    let is_enforced = client_id.is_some_and(|id| schema.is_enforced(id));

    // Non-enforced clients: store as-is, no validation.
    if !is_enforced {
        return Ok(());
    }

    // Collect provided keys with non-empty values for required-field checking.
    let provided_keys: std::collections::HashSet<&str> = entries
        .iter()
        .filter(|e| !e.value.is_empty())
        .map(|e| e.key.as_str())
        .collect();

    // Check all required fields are present.
    // BTreeMap iterates in sorted order, so error messages are deterministic.
    let missing: Vec<_> = schema
        .fields
        .iter()
        .filter(|(name, f)| f.required && !provided_keys.contains(name.as_str()))
        .map(|(name, f)| {
            if f.description.is_empty() {
                format!("  - {}", name)
            } else {
                format!("  - {}: {}", name, f.description)
            }
        })
        .collect();

    if !missing.is_empty() {
        return Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Input,
            "Missing required agent-context field(s):\n{}",
            missing.join("\n")
        ));
    }

    // Enforced clients: validate each entry.
    for entry in entries {
        let key = &entry.key;
        let value = &entry.value;

        // Skip keys injected from BUCK2_CLIENT_METADATA env var —
        // these are not user-provided --agent-context fields.
        if AgentContextEntry::ENV_INJECTED_KEYS.contains(&key.as_str()) {
            continue;
        }

        match schema.fields.get(key.as_str()) {
            None => {
                let valid_keys: Vec<&str> = schema.fields.keys().map(|k| k.as_str()).collect();
                return Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "Unknown agent-context key `{}`.\n  Valid keys: {}",
                    key,
                    valid_keys.join(", ")
                ));
            }
            Some(field_schema) => {
                if !field_schema.allowed_values.is_empty()
                    && !field_schema.allowed_values.iter().any(|v| v == value)
                {
                    let desc = if field_schema.description.is_empty() {
                        String::new()
                    } else {
                        format!("\n  {}: {}", key, field_schema.description)
                    };
                    return Err(buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Invalid agent-context value `{}` for key `{}`.{}\n  Valid values: {}",
                        value,
                        key,
                        desc,
                        field_schema.allowed_values.join(", ")
                    ));
                }
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn schema_with_intent() -> AgentContextSchema {
        let mut fields = BTreeMap::new();
        fields.insert(
            "intent".to_owned(),
            FieldSchema {
                required: false,
                allowed_values: vec!["build".to_owned(), "test".to_owned()],
                description: String::new(),
            },
        );
        AgentContextSchema {
            enforced_clients: vec!["claude_code".to_owned()],
            fields,
        }
    }

    fn entry(key: &str, value: &str) -> buck2_data::AgentContextEntry {
        buck2_data::AgentContextEntry {
            key: key.to_owned(),
            value: value.to_owned(),
        }
    }

    #[test]
    fn test_env_injected_keys_bypass_validation() {
        let schema = schema_with_intent();
        let entries = vec![
            entry("id", "claude_code"),
            entry("invocation_id", "claude_code_invocation_abc123"),
            entry("intent", "test"),
        ];
        assert!(validate_agent_context(&schema, Some("claude_code"), &entries).is_ok());
    }

    #[test]
    fn test_env_injected_keys_alone_pass_validation() {
        let schema = schema_with_intent();
        let entries = vec![
            entry("id", "claude_code"),
            entry("invocation_id", "claude_code_invocation_abc123"),
        ];
        assert!(validate_agent_context(&schema, Some("claude_code"), &entries).is_ok());
    }

    #[test]
    fn test_unknown_key_still_rejected() {
        let schema = schema_with_intent();
        let entries = vec![entry("id", "claude_code"), entry("bogus_key", "value")];
        assert!(validate_agent_context(&schema, Some("claude_code"), &entries).is_err());
    }
}
