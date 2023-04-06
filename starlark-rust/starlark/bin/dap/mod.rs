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

use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;

use debugserver_types::*;
use dupe::Dupe;
use gazebo::prelude::*;
pub(crate) use library::*;
use serde_json::Map;
use serde_json::Value;
use starlark::debug::dap_capabilities;
use starlark::debug::prepare_dap_adapter;
use starlark::debug::resolve_breakpoints;
use starlark::debug::DapAdapter;
use starlark::debug::DapAdapterClient;
use starlark::debug::DapAdapterEvalHook;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::syntax::AstModule;

use crate::eval::dialect;
use crate::eval::globals;

mod library;

#[derive(Debug)]
struct Backend {
    adapter: Arc<dyn DapAdapter>,
    eval_wrapper: Mutex<Option<Box<dyn DapAdapterEvalHook>>>,
    client: Client,
    file: Mutex<Option<String>>,
}

impl DapAdapterClient for Client {
    fn event_stopped(&self) {
        self.event_stopped(StoppedEventBody {
            reason: "breakpoint".to_owned(),
            thread_id: Some(0),
            description: Some("Hello".to_owned()),
            all_threads_stopped: Some(true),
            preserve_focus_hint: None,
            text: None,
        });
    }
}

fn get_ast(source: &str) -> anyhow::Result<Arc<AstModule>> {
    Ok(Arc::new(AstModule::parse_file(
        Path::new(source),
        &dialect(),
    )?))
}

impl Backend {
    fn execute(&self, path: &str) {
        let client = self.client.dupe();
        let client2 = self.client.dupe();
        let wrapper = self.eval_wrapper.lock().unwrap().take().unwrap();
        let path = PathBuf::from(path);

        let go = move || -> anyhow::Result<String> {
            client.log(&format!("EVALUATION PREPARE: {}", path.display()));
            let ast = AstModule::parse_file(&path, &dialect())?;
            let module = Module::new();
            let globals = globals();
            let mut eval = Evaluator::new(&module);
            wrapper.add_dap_hooks(&mut eval);

            // No way to pass back success/failure to the caller
            client.log(&format!("EVALUATION START: {}", path.display()));
            let v = eval.eval_module(ast, &globals)?;
            let s = v.to_string();
            client.log(&format!("EVALUATION FINISHED: {}", path.display()));
            Ok(s)
        };

        thread::spawn(move || {
            let res = go();
            let output = match &res {
                Err(e) => format!("{:#}", e),
                Ok(v) => v.to_owned(),
            };
            client2.event_output(OutputEventBody {
                output,
                category: None,
                column: None,
                data: None,
                line: None,
                source: None,
                variables_reference: None,
            });
            client2.event_exited(ExitedEventBody {
                exit_code: if res.is_ok() { 0 } else { 1 },
            });
            client2.event_terminated(None);
        });
    }
}

impl DebugServer for Backend {
    fn initialize(&self, _: InitializeRequestArguments) -> anyhow::Result<Option<Capabilities>> {
        self.client.event_initialized(None);
        Ok(Some(dap_capabilities()))
    }

    fn set_breakpoints(
        &self,
        x: SetBreakpointsArguments,
    ) -> anyhow::Result<SetBreakpointsResponseBody> {
        let source = x.source.path.as_ref().unwrap();
        let resolved = resolve_breakpoints(&x, &*get_ast(source)?)?;
        self.adapter.set_breakpoints(source, &resolved)?;
        Ok(resolved.to_response())
    }

    fn set_exception_breakpoints(&self, _: SetExceptionBreakpointsArguments) -> anyhow::Result<()> {
        // We just assume that break on error is always useful
        Ok(())
    }

    fn launch(&self, _: LaunchRequestArguments, args: Map<String, Value>) -> anyhow::Result<()> {
        // Expecting program of type string
        match args.get("program") {
            Some(Value::String(path)) => {
                *self.file.lock().unwrap() = Some(path.to_owned());
                Ok(())
            }
            _ => Err(anyhow::anyhow!(
                "Couldn't find a program to launch, got args {:?}",
                args
            )),
        }
    }

    fn threads(&self) -> anyhow::Result<ThreadsResponseBody> {
        Ok(ThreadsResponseBody {
            threads: vec![Thread {
                id: 0,
                name: "main".to_owned(),
            }],
        })
    }

    fn configuration_done(&self) -> anyhow::Result<()> {
        if let Some(path) = self.file.lock().unwrap().as_ref() {
            self.execute(path);
        }
        Ok(())
    }

    fn stack_trace(&self, v: StackTraceArguments) -> anyhow::Result<StackTraceResponseBody> {
        self.adapter.stack_trace(v)
    }

    fn scopes(&self, _: ScopesArguments) -> anyhow::Result<ScopesResponseBody> {
        let scopes_info = self.adapter.scopes()?;
        Ok(ScopesResponseBody {
            scopes: vec![Scope {
                name: "Locals".to_owned(),
                named_variables: Some(scopes_info.num_locals as i64),
                variables_reference: 2000,
                expensive: false,
                column: None,
                end_column: None,
                end_line: None,
                indexed_variables: None,
                line: None,
                source: None,
            }],
        })
    }

    fn variables(&self, _: VariablesArguments) -> anyhow::Result<VariablesResponseBody> {
        let vars_info = self.adapter.variables()?;
        Ok(VariablesResponseBody {
            variables: vars_info.locals.into_map(|var| var.to_dap()),
        })
    }

    fn evaluate(&self, x: EvaluateArguments) -> anyhow::Result<EvaluateResponseBody> {
        self.adapter.evaluate(&x.expression)
    }

    fn continue_(&self, x: ContinueArguments) -> anyhow::Result<ContinueResponseBody> {
        self.adapter.continue_(x)
    }
}

pub(crate) fn server() {
    DapService::run(|client| {
        let (adapter, wrapper) = prepare_dap_adapter(Box::new(client.dupe()));
        Backend {
            adapter: Arc::new(adapter),
            eval_wrapper: Mutex::new(Some(Box::new(wrapper))),
            client,
            file: Default::default(),
        }
    })
}
