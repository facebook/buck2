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

use debugserver_types::*;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

pub(crate) trait DebugServer {
    fn initialize(&self, x: InitializeRequestArguments) -> anyhow::Result<Option<Capabilities>>;
    fn set_breakpoints(
        &self,
        x: SetBreakpointsArguments,
    ) -> anyhow::Result<SetBreakpointsResponseBody>;
    fn set_exception_breakpoints(&self, x: SetExceptionBreakpointsArguments) -> anyhow::Result<()>;
    fn launch(&self, x: LaunchRequestArguments, args: Map<String, Value>) -> anyhow::Result<()>;
    fn threads(&self) -> anyhow::Result<ThreadsResponseBody>;
    fn configuration_done(&self) -> anyhow::Result<()>;
    fn stack_trace(&self, x: StackTraceArguments) -> anyhow::Result<StackTraceResponseBody>;
    fn scopes(&self, x: ScopesArguments) -> anyhow::Result<ScopesResponseBody>;
    fn variables(&self, x: VariablesArguments) -> anyhow::Result<VariablesResponseBody>;
    fn continue_(&self, x: ContinueArguments) -> anyhow::Result<ContinueResponseBody>;
    fn evaluate(&self, x: EvaluateArguments) -> anyhow::Result<EvaluateResponseBody>;
    fn disconnect(&self, _x: DisconnectArguments) -> anyhow::Result<()> {
        Ok(())
    }
}

pub(crate) fn dispatch(server: &impl DebugServer, r: &Request) -> Response {
    fn arg<T: for<'a> Deserialize<'a>>(r: &Request) -> T {
        serde_json::from_value(r.arguments.clone().unwrap()).unwrap()
    }

    fn arg_extra(r: &Request) -> Map<String, Value> {
        match &r.arguments {
            Some(Value::Object(x)) => x.clone(),
            _ => Default::default(),
        }
    }

    fn ret<T: Serialize>(r: &Request, v: anyhow::Result<Option<T>>) -> Response {
        Response {
            type_: "response".to_owned(),
            command: r.command.clone(),
            request_seq: r.seq,
            seq: 0,
            success: v.is_ok(),
            message: v.as_ref().err().map(|e| format!("{:#}", e)),
            body: v.unwrap_or(None).map(|v| serde_json::to_value(v).unwrap()),
        }
    }

    fn ret_some<T: Serialize>(r: &Request, v: anyhow::Result<T>) -> Response {
        ret(r, v.map(Some))
    }

    fn ret_none(r: &Request, v: anyhow::Result<()>) -> Response {
        ret::<()>(r, v.map(|_| None))
    }

    match r.command.as_str() {
        "initialize" => ret(r, server.initialize(arg(r))),
        "setBreakpoints" => ret_some(r, server.set_breakpoints(arg(r))),
        "setExceptionBreakpoints" => ret_none(r, server.set_exception_breakpoints(arg(r))),
        "launch" => ret_none(r, server.launch(arg(r), arg_extra(r))),
        "threads" => ret_some(r, server.threads()),
        "configurationDone" => ret_none(r, server.configuration_done()),
        "stackTrace" => ret_some(r, server.stack_trace(arg(r))),
        "scopes" => ret_some(r, server.scopes(arg(r))),
        "variables" => ret_some(r, server.variables(arg(r))),
        "continue" => ret_some(r, server.continue_(arg(r))),
        "evaluate" => ret_some(r, server.evaluate(arg(r))),
        "disconnect" => ret_none(r, server.disconnect(arg(r))),
        _ => ret_none(r, Err(anyhow::anyhow!("Unknown command: {}", r.command))),
    }
}
