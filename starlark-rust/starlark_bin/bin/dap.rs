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
pub(crate) use library::*;
use serde_json::Map;
use serde_json::Value;
use starlark::StarlarkResultExt;
use starlark::debug::DapAdapter;
use starlark::debug::DapAdapterClient;
use starlark::debug::DapAdapterEvalHook;
use starlark::debug::dap_capabilities;
use starlark::debug::prepare_dap_adapter;
use starlark::debug::resolve_breakpoints;
use starlark::environment::Globals;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::syntax::AstModule;
use starlark::syntax::Dialect;

mod library;

#[derive(Debug)]
struct Backend {
    adapter: Arc<dyn DapAdapter>,
    eval_wrapper: Mutex<Option<Box<dyn DapAdapterEvalHook>>>,
    client: Client,
    file: Mutex<Option<String>>,
    dialect: Dialect,
    globals: Globals,
}

impl DapAdapterClient for Client {
    fn event_stopped(&self) -> starlark::Result<()> {
        self.event_stopped(StoppedEventBody {
            reason: "breakpoint".to_owned(),
            thread_id: Some(0),
            description: Some("Hello".to_owned()),
            all_threads_stopped: Some(true),
            preserve_focus_hint: None,
            text: None,
        });
        Ok(())
    }
}

impl Backend {
    fn execute(&self, path: &str) {
        let client = self.client.dupe();
        let client2 = self.client.dupe();
        let wrapper = self.eval_wrapper.lock().unwrap().take().unwrap();
        let path = PathBuf::from(path);
        let dialect = self.dialect.clone();
        let globals = self.globals.dupe();

        let go = move || -> anyhow::Result<String> {
            client.log(&format!("EVALUATION PREPARE: {}", path.display()));
            let ast = AstModule::parse_file(&path, &dialect).into_anyhow_result()?;
            Module::with_temp_heap(|module| {
                let mut eval = Evaluator::new(&module);
                wrapper.add_dap_hooks(&mut eval);

                // No way to pass back success/failure to the caller
                client.log(&format!("EVALUATION START: {}", path.display()));
                let v = eval.eval_module(ast, &globals).into_anyhow_result()?;
                let s = v.to_string();
                client.log(&format!("EVALUATION FINISHED: {}", path.display()));
                Ok::<_, anyhow::Error>(s)
            })
        };

        thread::spawn(move || {
            let res = go();
            let output = match &res {
                Err(e) => format!("{e:#}"),
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

    fn get_ast(&self, source: &str) -> anyhow::Result<Arc<AstModule>> {
        Ok(Arc::new(
            AstModule::parse_file(Path::new(source), &self.dialect).into_anyhow_result()?,
        ))
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
        let resolved = resolve_breakpoints(&x, &*self.get_ast(source)?)?;
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
            variables: vars_info
                .locals
                .into_iter()
                .map(|var| var.to_dap())
                .collect(),
        })
    }

    fn evaluate(&self, x: EvaluateArguments) -> anyhow::Result<EvaluateResponseBody> {
        let expr_result = self.adapter.evaluate(&x.expression)?;

        Ok(EvaluateResponseBody {
            indexed_variables: None,
            named_variables: None,
            presentation_hint: None,
            result: expr_result.result,
            type_: Some(expr_result.type_),
            variables_reference: 0.0,
        })
    }

    fn continue_(&self, _: ContinueArguments) -> anyhow::Result<ContinueResponseBody> {
        self.adapter.continue_()?;
        Ok(ContinueResponseBody::default())
    }
}

pub(crate) fn server(dialect: Dialect, globals: Globals) {
    DapService::run(|client| {
        let (adapter, wrapper) = prepare_dap_adapter(Box::new(client.dupe()));
        Backend {
            adapter: Arc::new(adapter),
            eval_wrapper: Mutex::new(Some(Box::new(wrapper))),
            client,
            file: Default::default(),
            dialect,
            globals,
        }
    })
}
