/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_cli_proto::new_generic::DocsResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::agent_context::AgentContextSchema;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use dice::DiceTransaction;

pub(crate) async fn docs_agent(dice_ctx: DiceTransaction) -> buck2_error::Result<DocsResponse> {
    let root_cell = dice_ctx.ctx().get_cell_resolver().await?.root_cell();
    let root_config = dice_ctx.ctx().get_legacy_config_for_cell(root_cell).await?;
    let schema = AgentContextSchema::from_config(root_config);

    Ok(DocsResponse::Text(render_agent_context_schema(&schema)))
}

fn render_agent_context_schema(schema: &AgentContextSchema) -> String {
    if schema.is_empty() {
        return [
            "For builds at Meta, use Buck2.",
            "",
            "No repository-defined `--agent-context` schema is configured, so there is no need to pass `--agent-context`.",
        ]
        .join("\n");
    }

    [
        "For builds at Meta, use Buck2.".to_owned(),
        String::new(),
        "# Rules".to_owned(),
        String::new(),
        "ALWAYS pass `--agent-context key=value` to every `buck2` or `buck` invocation, following the repository-defined schema below. For multiple entries, repeat the flag: `--agent-context key1=value1 --agent-context key2=value2`.".to_owned(),
        String::new(),
        "# `--agent-context` schema".to_owned(),
        String::new(),
        schema.to_string(),
    ]
    .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_schema_does_not_require_agent_context() {
        assert_eq!(
            "For builds at Meta, use Buck2.\n\nNo repository-defined `--agent-context` schema is configured, so there is no need to pass `--agent-context`.",
            render_agent_context_schema(&AgentContextSchema::default()),
        );
    }
}
