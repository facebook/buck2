/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This contains the [DebugServer] trait which represents the part of the debug adapter
//! protocol implemented by the buck debug server and some utilities for working with
//! the debug adapter response/request/etc types.

use debugserver_types as dap;
use serde::Deserialize;
use serde::Serialize;

// TODO(cjhopman): debugserver_types is pretty out of date, we should switch to another
// crate (debug_types looked more up to date). Also it'd probably be nice to use a crate
// that has the dap types pre-generated (i.e. as part of the source) rather than
// one that generates them at build time so that the IDE experience is better.

/// This trait contains all the debug adapter requests that our debugserver supports.
///
/// See <https://microsoft.github.io/debug-adapter-protocol/specification> for the specification
/// of the debug adapter protocol.
///
/// This trait only has the one implementation, but it's split out here to make it clearer
/// what part of the protocol is implemented and for helpers like `dispatch` not needing to
/// handle the concrete type.
pub(crate) trait DebugServer {
    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Initialize>
    fn initialize(
        &mut self,
        x: dap::InitializeRequestArguments,
    ) -> buck2_error::Result<Option<serde_json::Value>>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_SetBreakpoints>
    fn set_breakpoints(
        &mut self,
        x: dap::SetBreakpointsArguments,
    ) -> buck2_error::Result<dap::SetBreakpointsResponseBody>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_SetExceptionBreakpoints>
    fn set_exception_breakpoints(
        &mut self,
        x: dap::SetExceptionBreakpointsArguments,
    ) -> buck2_error::Result<()>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Attach>
    fn attach(&mut self, x: dap::AttachRequestArguments) -> buck2_error::Result<()>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Threads>
    fn threads(&mut self) -> buck2_error::Result<dap::ThreadsResponseBody>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_ConfigurationDone>
    fn configuration_done(&mut self) -> buck2_error::Result<()>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_StackTrace>
    fn stack_trace(
        &mut self,
        x: dap::StackTraceArguments,
    ) -> buck2_error::Result<dap::StackTraceResponseBody>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Scopes>
    fn scopes(&mut self, x: dap::ScopesArguments) -> buck2_error::Result<dap::ScopesResponseBody>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Variables>
    fn variables(
        &mut self,
        x: dap::VariablesArguments,
    ) -> buck2_error::Result<dap::VariablesResponseBody>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Continue>
    fn continue_(&mut self, x: ContinueArguments)
    -> buck2_error::Result<dap::ContinueResponseBody>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Next>
    fn next(&mut self, x: dap::NextArguments) -> buck2_error::Result<()>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_StepIn>
    fn step_in(&mut self, x: dap::StepInArguments) -> buck2_error::Result<()>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_StepOut>
    fn step_out(&mut self, x: dap::StepOutArguments) -> buck2_error::Result<()>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Evaluate>
    fn evaluate(
        &mut self,
        x: dap::EvaluateArguments,
    ) -> buck2_error::Result<dap::EvaluateResponseBody>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Disconnect>
    fn disconnect(&mut self, x: dap::DisconnectArguments) -> buck2_error::Result<()>;

    /// See <https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Source>
    fn source(&mut self, x: dap::SourceArguments) -> buck2_error::Result<dap::SourceResponseBody>;
}

/// DAP ContinueArguments (debugserver_types is missing single_thread field)
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ContinueArguments {
    pub(crate) thread_id: i64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) single_thread: Option<bool>,
}

/// Create a dap Event with the given body. The user is responsible for updating the `seq` field.
pub(crate) fn dap_event<T: Serialize>(event: &str, body: Option<&T>) -> dap::Event {
    dap::Event {
        type_: "event".to_owned(),
        seq: 0,
        event: event.to_owned(),
        body: body.map(|v| serde_json::json!(v)),
    }
}

/// Create a Response for the Request indicating an error. The user is responsible for updating the `seq` field.
pub(crate) fn err_response(req: &dap::Request, err: &buck2_error::Error) -> dap::Response {
    dap::Response {
        type_: "response".to_owned(),
        command: req.command.clone(),
        request_seq: req.seq,
        seq: 0,
        success: false,
        message: Some(format!("{:#}", err)),
        body: None,
    }
}

pub(crate) fn dispatch(
    server: &mut impl DebugServer,
    r: &dap::Request,
) -> buck2_error::Result<dap::Response> {
    fn arg<T: for<'a> Deserialize<'a>>(r: &dap::Request) -> buck2_error::Result<T> {
        Ok(serde_json::from_value(r.arguments.clone().ok_or_else(
            || {
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "missing expected argument in DAP request"
                )
            },
        )?)?)
    }

    fn ret<T: Serialize>(
        r: &dap::Request,
        v: buck2_error::Result<Option<T>>,
    ) -> buck2_error::Result<dap::Response> {
        let v = v?;
        Ok(dap::Response {
            type_: "response".to_owned(),
            command: r.command.clone(),
            request_seq: r.seq,
            seq: 0,
            success: true,
            message: None,
            body: v.map(|v| serde_json::to_value(v).unwrap()),
        })
    }

    fn ret_some<T: Serialize>(
        r: &dap::Request,
        v: buck2_error::Result<T>,
    ) -> buck2_error::Result<dap::Response> {
        ret(r, v.map(|v| Some(v)))
    }

    fn ret_none(
        r: &dap::Request,
        v: buck2_error::Result<()>,
    ) -> buck2_error::Result<dap::Response> {
        ret::<()>(r, v.map(|_| None))
    }

    match r.command.as_str() {
        "initialize" => ret(r, server.initialize(arg(r)?)),
        "setBreakpoints" => ret_some(r, server.set_breakpoints(arg(r)?)),
        "setExceptionBreakpoints" => ret_none(r, server.set_exception_breakpoints(arg(r)?)),
        "attach" => ret_none(r, server.attach(arg(r)?)),
        "threads" => ret_some(r, server.threads()),
        "configurationDone" => ret_none(r, server.configuration_done()),
        "stackTrace" => ret_some(r, server.stack_trace(arg(r)?)),
        "scopes" => ret_some(r, server.scopes(arg(r)?)),
        "variables" => ret_some(r, server.variables(arg(r)?)),
        "continue" => ret_some(r, server.continue_(arg(r)?)),
        "evaluate" => ret_some(r, server.evaluate(arg(r)?)),
        "disconnect" => ret_none(r, server.disconnect(arg(r)?)),
        "source" => ret_some(r, server.source(arg(r)?)),
        "next" => ret_none(r, server.next(arg(r)?)),
        "stepIn" => ret_none(r, server.step_in(arg(r)?)),
        "stepOut" => ret_none(r, server.step_out(arg(r)?)),
        _ => Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Input,
            "Buck2 debugserver didn't recognize command: {}",
            r.command
        )),
    }
}
