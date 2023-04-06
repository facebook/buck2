/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! Provides utilities useful for implementation of the debug adapter protocol (DAP, see
//! <https://microsoft.github.io/debug-adapter-protocol/>), primarily the DapAdapter/DapAdapterEvalHook
//! that provide for debugging a starlark Evaluation.

use std::fmt::Debug;

use debugserver_types::*;
use dupe::Dupe;

use crate::codemap::FileSpan;
use crate::eval::Evaluator;
use crate::syntax::AstModule;

mod implementation;
mod tests;

/// The DapAdapterClient is implemented by the user and provides functionality required by the DapAdapter.
pub trait DapAdapterClient: Debug + Send + Sync + 'static {
    /// Indicates that the evaluation stopped at a breakpoint.
    fn event_stopped(&self);
}

/// Information about the variables scopes
pub struct ScopesInfo {
    /// Number of local variables.
    pub num_locals: usize,
}

/// Information about a variable.
pub struct Variable {
    /// Name of the variable.
    pub name: String,
    /// The value as a String.
    pub value: String,
    /// The variables type.
    pub type_: String,
}

impl Variable {
    /// Helper to convert to the DAP Variable type.
    pub fn to_dap(self) -> debugserver_types::Variable {
        debugserver_types::Variable {
            name: self.name,
            value: self.value,
            type_: Some(self.type_),
            evaluate_name: None,
            indexed_variables: None,
            named_variables: None,
            presentation_hint: None,
            variables_reference: 0,
        }
    }
}

/// The kind of debugger step, used for next/stepin/stepout requests.
#[derive(Debug, Clone, Dupe, Copy)]
pub enum StepKind {
    /// Step "into" the statement. This is generally used on a function call to stop in the
    /// function call. In practice, this will stop on the next statement.
    Into,
    /// Step "over" the statement. This will stop on the next statement in the current function
    /// after the current one (so will step "over" a function call).
    Over,
    /// Step "out" of the current function. This will stop on the next statement after this
    /// function returns.
    Out,
}

/// Information about variables in scope.
pub struct VariablesInfo {
    /// Local variables.
    pub locals: Vec<Variable>,
}

/// The DapAdapter accepts DAP requests and updates the hooks in the running evaluator.
pub trait DapAdapter: Debug + Send + 'static {
    /// Sets multiple breakpoints for a file (and clears existing ones).
    ///
    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_SetBreakpoints>
    fn set_breakpoints(
        &self,
        source: &str,
        breakpoints: &ResolvedBreakpoints,
    ) -> anyhow::Result<()>;

    /// Gets the top stack frame, may be None if entered from native.
    fn top_frame(&self) -> anyhow::Result<Option<StackFrame>>;

    /// Gets a stacktrace from the current execution state.
    ///
    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_StackTrace>
    fn stack_trace(&self, args: StackTraceArguments) -> anyhow::Result<StackTraceResponseBody>;

    /// Gets the variables scope for a frame.
    ///
    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Scopes>
    fn scopes(&self) -> anyhow::Result<ScopesInfo>;

    /// Gets child variables for a variable reference.
    ///
    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Variables>
    fn variables(&self) -> anyhow::Result<VariablesInfo>;

    /// Resumes execution.
    ///
    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Continue>
    fn continue_(&self) -> anyhow::Result<()>;

    /// Continues execution until some condition.
    ///
    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Next>
    /// <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_StepIn>
    /// <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_StepOut>
    fn step(&self, kind: StepKind) -> anyhow::Result<()>;
    /// Evaluates in expression in the context of the top-most frame.
    ///
    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Evaluate>
    fn evaluate(&self, expr: &str) -> anyhow::Result<EvaluateResponseBody>;
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Breakpoint {
    span: FileSpan,
    condition: Option<String>,
}

/// Breakpoints resolved to their spans.
#[derive(Debug)]
pub struct ResolvedBreakpoints(Vec<Option<Breakpoint>>);

impl ResolvedBreakpoints {
    /// Converts resolved breakpoints to a SetBreakpointsResponseBody. The breakpoints should've been resolved from the corresponding SetBreakpointsRequest.
    pub fn to_response(&self) -> SetBreakpointsResponseBody {
        implementation::resolved_breakpoints_to_dap(self)
    }
}

/// Resolves the breakpoints to their FileSpan if possible.
pub fn resolve_breakpoints(
    args: &SetBreakpointsArguments,
    ast: &AstModule,
) -> anyhow::Result<ResolvedBreakpoints> {
    implementation::resolve_breakpoints(args, ast)
}

/// This is sort of the evaluation side of the DapAdapter. It's expected that these are on different threads
/// (the starlark evaluation is single-threaded, so certainly the DapAdapter itself doesn't do interesting
/// things there).
pub trait DapAdapterEvalHook: Debug + Send + 'static {
    /// Hooks the evaluator for this DapAdapter.
    fn add_dap_hooks<'v, 'a>(self: Box<Self>, eval: &mut Evaluator<'v, 'a>);
}

/// The DAP capabilities that the adapter supports.
pub fn dap_capabilities() -> Capabilities {
    Capabilities {
        supports_configuration_done_request: Some(true),
        supports_evaluate_for_hovers: Some(true),
        supports_set_variable: Some(true),
        supports_step_in_targets_request: Some(true),
        supports_conditional_breakpoints: Some(true),
        ..Capabilities::default()
    }
}

/// Creates a DapAdapter and corresponding DapAdapterEvalHook.
pub fn prepare_dap_adapter(
    client: Box<dyn DapAdapterClient>,
) -> (impl DapAdapter, impl DapAdapterEvalHook) {
    implementation::prepare_dap_adapter(client)
}
