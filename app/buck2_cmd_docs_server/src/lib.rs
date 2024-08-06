/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]

use std::collections::HashMap;

use async_trait::async_trait;
use buck2_cli_proto::new_generic::DocsRequest;
use buck2_cli_proto::new_generic::DocsResponse;
use buck2_interpreter_for_build::interpreter::globals::register_analysis_natives;
use buck2_interpreter_for_build::interpreter::globals::register_bxl_natives;
use buck2_interpreter_for_build::interpreter::globals::register_load_natives;
use buck2_interpreter_for_build::interpreter::globals::starlark_library_extensions_for_buck2;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::late_bindings::DocsServerComamnd;
use buck2_server_ctx::late_bindings::DOCS_SERVER_COMMAND;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use dice::DiceTransaction;
use starlark::docs::get_registered_starlark_docs;
use starlark::docs::Doc;
use starlark::docs::DocItem;
use starlark::docs::DocModule;
use starlark::docs::Identifier;
use starlark::environment::Globals;
use starlark::environment::GlobalsBuilder;

use crate::builtins::docs_starlark_builtins;
use crate::starlark_::docs_starlark;

mod builtins;
mod json;
mod markdown;
mod starlark_;

fn builtin_doc<S: ToString>(name: S, directory: &str, module: DocModule) -> Doc {
    let mut custom_attrs = HashMap::new();
    if !directory.is_empty() {
        custom_attrs.insert("directory".to_owned(), directory.to_owned());
    }

    Doc {
        id: Identifier {
            name: name.to_string(),
            location: None,
        },
        item: DocItem::Module(module),
        custom_attrs,
    }
}

fn get_builtin_global_starlark_docs() -> DocModule {
    Globals::extended_by(starlark_library_extensions_for_buck2()).documentation()
}

/// Globals that are in the interpreter (including BXL), but none of the starlark global symbols.
fn get_builtin_build_docs() -> DocModule {
    GlobalsBuilder::new()
        .with(register_load_natives)
        .with(register_analysis_natives)
        .with(register_bxl_natives)
        .build()
        .documentation()
}

fn get_builtin_docs() -> Vec<Doc> {
    let mut all_builtins = vec![
        builtin_doc("globals", "standard", get_builtin_global_starlark_docs()),
        builtin_doc("globals", "", get_builtin_build_docs()),
    ];

    all_builtins.extend(get_registered_starlark_docs());

    all_builtins
}

struct DocsServerCommandImpl;

#[async_trait::async_trait]
impl DocsServerComamnd for DocsServerCommandImpl {
    async fn docs(
        &self,
        context: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: DocsRequest,
    ) -> anyhow::Result<DocsResponse> {
        run_server_command(
            DocsServerCommand { req },
            context,
            partial_result_dispatcher,
        )
        .await
    }
}

pub fn init_late_bindings() {
    DOCS_SERVER_COMMAND.init(&DocsServerCommandImpl);
}

struct DocsServerCommand {
    req: DocsRequest,
}

#[async_trait]
impl ServerCommandTemplate for DocsServerCommand {
    type StartEvent = buck2_data::DocsCommandStart;
    type EndEvent = buck2_data::DocsCommandEnd;
    type Response = DocsResponse;
    type PartialResult = NoPartialResult;

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        docs(server_ctx, ctx, &self.req).await
    }

    fn is_success(&self, _response: &Self::Response) -> bool {
        // No response if we failed.
        true
    }
}

async fn docs(
    server_ctx: &dyn ServerCommandContextTrait,
    dice_ctx: DiceTransaction,
    request: &DocsRequest,
) -> anyhow::Result<DocsResponse> {
    match request {
        DocsRequest::Starlark(request) => docs_starlark(server_ctx, dice_ctx, request).await,
        DocsRequest::StarlarkBuiltins(request) => {
            docs_starlark_builtins(server_ctx, dice_ctx, request).await
        }
    }
}
