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

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc::{channel, Receiver, Sender},
        Arc, Mutex,
    },
    thread,
};

use debugserver_types::*;
use gazebo::prelude::*;
pub(crate) use library::*;
use serde_json::{Map, Value};
use starlark::{
    codemap::{FileSpan, FileSpanRef},
    environment::Module,
    eval::Evaluator,
    syntax::{AstModule, Dialect},
};

use crate::eval::{dialect, globals};

mod library;

#[derive(Debug)]
struct Backend {
    client: Client,
    file: Mutex<Option<String>>,

    // These breakpoints must all match statements as per before_stmt.
    // Those values for which we abort the execution.
    breakpoints: Arc<Mutex<HashMap<String, HashSet<FileSpan>>>>,
    // Set while we are doing evaluate calls (>= 1 means disable)
    disable_breakpoints: Arc<AtomicUsize>,

    sender: Sender<Box<dyn Fn(FileSpanRef, &mut Evaluator) -> Next + Send>>,
    receiver: Arc<Mutex<Receiver<Box<dyn Fn(FileSpanRef, &mut Evaluator) -> Next + Send>>>>,
}

enum Next {
    Continue,
    RemainPaused,
}

impl Backend {
    fn inject<T: 'static + Send>(
        &self,
        f: Box<dyn Fn(FileSpanRef, &mut Evaluator) -> (Next, T) + Send>,
    ) -> T {
        let (sender, receiver) = channel();
        self.sender
            .send(box move |span, eval| {
                let (next, res) = f(span, eval);
                sender.send(res).unwrap();
                next
            })
            .unwrap();
        receiver.recv().unwrap()
    }

    fn inject_continue(&self) {
        self.inject(box |_, _| (Next::Continue, ()))
    }

    fn with_ctx<T: 'static + Send>(
        &self,
        f: Box<dyn Fn(FileSpanRef, &mut Evaluator) -> T + Send>,
    ) -> T {
        self.inject(box move |span, eval| (Next::RemainPaused, f(span, eval)))
    }

    fn execute(&self, path: &str) {
        let client = self.client.dupe();
        let client2 = self.client.dupe();
        let path = PathBuf::from(path);
        let breakpoints = self.breakpoints.dupe();
        let disable_breakpoints = self.disable_breakpoints.dupe();
        let receiver = self.receiver.dupe();

        let go = move || -> anyhow::Result<String> {
            client.log(&format!("EVALUATION PREPARE: {}", path.display()));
            let ast = AstModule::parse_file(&path, &dialect())?;
            let module = Module::new();
            let globals = globals();
            let mut eval = Evaluator::new(&module);
            let fun = |span_loc: FileSpanRef, eval: &mut Evaluator| {
                let stop = if disable_breakpoints.load(Ordering::SeqCst) > 0 {
                    false
                } else {
                    let breaks = breakpoints.lock().unwrap();
                    breaks
                        .get(span_loc.filename())
                        .map(|set| set.contains(&span_loc.to_file_span()))
                        .unwrap_or_default()
                };
                if stop {
                    client.event_stopped(StoppedEventBody {
                        reason: "breakpoint".to_owned(),
                        thread_id: Some(0),
                        description: Some("Hello".to_owned()),
                        all_threads_stopped: Some(true),
                        preserve_focus_hint: None,
                        text: None,
                    });
                    loop {
                        let msg = receiver.lock().unwrap().recv().unwrap();
                        match msg(span_loc, eval) {
                            Next::Continue => break,
                            Next::RemainPaused => continue,
                        }
                    }
                }
            };
            eval.before_stmt_for_dap(&fun);
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

fn breakpoint(verified: bool) -> Breakpoint {
    Breakpoint {
        column: None,
        end_column: None,
        end_line: None,
        id: None,
        line: None,
        message: None,
        source: None,
        verified,
    }
}

impl DebugServer for Backend {
    fn initialize(&self, _: InitializeRequestArguments) -> anyhow::Result<Option<Capabilities>> {
        self.client.event_initialized(None);
        Ok(Some(Capabilities {
            supports_configuration_done_request: Some(true),
            supports_evaluate_for_hovers: Some(true),
            supports_set_variable: Some(true),
            supports_step_in_targets_request: Some(true),
            ..Capabilities::default()
        }))
    }

    fn set_breakpoints(
        &self,
        x: SetBreakpointsArguments,
    ) -> anyhow::Result<SetBreakpointsResponseBody> {
        let breakpoints = x.breakpoints.unwrap_or_default();
        let source = x.source.path.unwrap();

        if breakpoints.is_empty() {
            self.breakpoints.lock().unwrap().remove(&source);
            Ok(SetBreakpointsResponseBody {
                breakpoints: Vec::new(),
            })
        } else {
            match AstModule::parse_file(Path::new(&source), &dialect()) {
                Err(_) => {
                    self.breakpoints.lock().unwrap().remove(&source);
                    Ok(SetBreakpointsResponseBody {
                        breakpoints: vec![breakpoint(false); breakpoints.len()],
                    })
                }
                Ok(ast) => {
                    let poss: HashMap<usize, FileSpan> = ast
                        .stmt_locations()
                        .iter()
                        .map(|span| (span.resolve_span().begin_line, span.dupe()))
                        .collect();
                    let list = breakpoints.map(|x| poss.get(&(x.line as usize - 1)));
                    self.breakpoints
                        .lock()
                        .unwrap()
                        .insert(source, list.iter().filter_map(|x| x.duped()).collect());
                    Ok(SetBreakpointsResponseBody {
                        breakpoints: list.map(|x| breakpoint(x.is_some())),
                    })
                }
            }
        }
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

    fn stack_trace(&self, _: StackTraceArguments) -> anyhow::Result<StackTraceResponseBody> {
        fn convert_frame(id: usize, name: String, location: Option<FileSpan>) -> StackFrame {
            let mut s = StackFrame {
                id: id as i64,
                name,
                column: 0,
                line: 0,
                end_column: None,
                end_line: None,
                module_id: None,
                presentation_hint: None,
                source: None,
            };
            if let Some(loc) = location {
                let span = loc.resolve_span();
                s.line = span.begin_line as i64 + 1;
                s.column = span.begin_column as i64 + 1;
                s.end_line = Some(span.end_line as i64 + 1);
                s.end_column = Some(span.end_column as i64 + 1);
                s.source = Some(Source {
                    path: Some(loc.filename().to_owned()),
                    ..Source::default()
                })
            }
            s
        }

        // Our model of a Frame and the debugger model are a bit different.
        // We record the location of the call, but DAP wants the location we are at.
        // We also have them in the wrong order
        self.with_ctx(box |span, eval| {
            let frames = eval.call_stack().into_frames();
            let mut next = Some(span.to_file_span());
            let mut res = Vec::with_capacity(frames.len() + 1);
            for (i, x) in frames.iter().rev().enumerate() {
                res.push(convert_frame(i, x.name.clone(), next));
                next = x.location.dupe();
            }
            res.push(convert_frame(10000, "Root".to_owned(), next));
            Ok(StackTraceResponseBody {
                total_frames: Some(res.len() as i64),
                stack_frames: res,
            })
        })
    }

    fn scopes(&self, _: ScopesArguments) -> anyhow::Result<ScopesResponseBody> {
        self.with_ctx(box |_, eval| {
            let vars = eval.local_variables();
            Ok(ScopesResponseBody {
                scopes: vec![Scope {
                    name: "Locals".to_owned(),
                    named_variables: Some(vars.len() as i64),
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
        })
    }

    fn variables(&self, _: VariablesArguments) -> anyhow::Result<VariablesResponseBody> {
        self.with_ctx(box |_, eval| {
            let vars = eval.local_variables();
            Ok(VariablesResponseBody {
                variables: vars
                    .into_iter()
                    .map(|(name, value)| Variable {
                        name,
                        value: value.to_string(),
                        type_: Some(value.get_type().to_owned()),
                        evaluate_name: None,
                        indexed_variables: None,
                        named_variables: None,
                        presentation_hint: None,
                        variables_reference: 0,
                    })
                    .collect(),
            })
        })
    }

    fn continue_(&self, _: ContinueArguments) -> anyhow::Result<ContinueResponseBody> {
        self.inject_continue();
        Ok(ContinueResponseBody::default())
    }

    fn evaluate(&self, x: EvaluateArguments) -> anyhow::Result<EvaluateResponseBody> {
        let disable_breakpoints = self.disable_breakpoints.dupe();
        self.with_ctx(box move |_, eval| {
            // We don't want to trigger breakpoints during an evaluate,
            // not least because we currently don't allow reenterant evaluate
            disable_breakpoints.fetch_add(1, Ordering::SeqCst);
            let ast = AstModule::parse("interactive", x.expression.clone(), &Dialect::Extended);
            let s = match ast.and_then(|ast| eval.eval_statements(ast)) {
                Err(e) => format!("{:#}", e),
                Ok(v) => v.to_string(),
            };
            disable_breakpoints.fetch_sub(1, Ordering::SeqCst);
            Ok(EvaluateResponseBody {
                indexed_variables: None,
                named_variables: None,
                presentation_hint: None,
                result: s,
                type_: None,
                variables_reference: 0.0,
            })
        })
    }
}

pub(crate) fn server() {
    let (sender, receiver) = channel();
    DapService::run(|client| Backend {
        client,
        breakpoints: Default::default(),
        disable_breakpoints: Default::default(),
        file: Default::default(),
        sender,
        receiver: Arc::new(Mutex::new(receiver)),
    })
}
