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

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::path::Path;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::mpsc::channel;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;
use std::sync::Arc;
use std::sync::Mutex;

use debugserver_types::*;
use dupe::Dupe;
use dupe::OptionDupedExt;
use gazebo::prelude::*;
use starlark::codemap::FileSpan;
use starlark::codemap::FileSpanRef;
use starlark::eval::BeforeStmtFuncDyn;
use starlark::eval::Evaluator;
use starlark::syntax::AstModule;
use starlark::syntax::Dialect;

use crate::eval::dialect;

pub trait DapAdapterClient: Debug + Send + Sync + 'static {
    fn log(&self, x: &str);

    fn event_stopped(&self);

    fn event_initialized(&self);
}

type ToEvalMessage = Box<dyn Fn(FileSpanRef, &mut Evaluator) -> Next + Send>;

#[derive(Debug)]
pub struct DapAdapter {
    state: Arc<SharedAdapterState>,
    sender: Sender<ToEvalMessage>,
}

pub struct DapAdapterEvaluationWrapper {
    state: Arc<SharedAdapterState>,
    receiver: Receiver<ToEvalMessage>,
}

impl<'a> BeforeStmtFuncDyn<'a> for DapAdapterEvaluationWrapper {
    fn call<'v>(&mut self, span_loc: FileSpanRef, eval: &mut Evaluator<'v, 'a>) {
        let stop = if self.state.disable_breakpoints.load(Ordering::SeqCst) > 0 {
            false
        } else {
            let breaks = self.state.breakpoints.lock().unwrap();
            breaks
                .get(span_loc.filename())
                .map(|set| set.contains(&span_loc.to_file_span()))
                .unwrap_or_default()
        };
        if stop {
            self.state.client.event_stopped();
            loop {
                let msg = self.receiver.recv().unwrap();
                match msg(span_loc, eval) {
                    Next::Continue => break,
                    Next::RemainPaused => continue,
                }
            }
        }
    }
}

static_assertions::assert_impl_all!(DapAdapter: Send);

impl Debug for DapAdapterEvaluationWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DapAdapterEvaluationWrapper").finish()
    }
}

impl DapAdapterEvaluationWrapper {
    fn new(state: Arc<SharedAdapterState>, receiver: Receiver<ToEvalMessage>) -> Self {
        Self { state, receiver }
    }

    pub fn wrap_for_dap<'v, 'a>(self: Box<Self>, mut eval: Evaluator<'v, 'a>) -> Evaluator<'v, 'a> {
        eval.before_stmt_for_dap((self as Box<dyn BeforeStmtFuncDyn>).into());
        eval
    }
}

#[derive(Debug)]
struct SharedAdapterState {
    client: Box<dyn DapAdapterClient>,
    // These breakpoints must all match statements as per before_stmt.
    // Those values for which we abort the execution.
    breakpoints: Arc<Mutex<HashMap<String, HashSet<FileSpan>>>>,
    // Set while we are doing evaluate calls (>= 1 means disable)
    disable_breakpoints: Arc<AtomicUsize>,
}

enum Next {
    Continue,
    RemainPaused,
}

impl DapAdapter {
    pub fn new(client: Box<dyn DapAdapterClient>) -> (Self, DapAdapterEvaluationWrapper) {
        let (sender, receiver) = std::sync::mpsc::channel::<ToEvalMessage>();
        let state = Arc::new(SharedAdapterState {
            client,
            breakpoints: Arc::new(Mutex::new(HashMap::new())),
            disable_breakpoints: Arc::new(0usize.into()),
        });

        (
            Self {
                state: state.clone(),
                sender,
            },
            DapAdapterEvaluationWrapper::new(state, receiver),
        )
    }

    fn inject<T: 'static + Send>(
        &self,
        f: Box<dyn Fn(FileSpanRef, &mut Evaluator) -> (Next, T) + Send>,
    ) -> T {
        let (sender, receiver) = channel();
        self.sender
            .send(Box::new(move |span, eval| {
                let (next, res) = f(span, eval);
                sender.send(res).unwrap();
                next
            }))
            .unwrap();
        receiver.recv().unwrap()
    }

    fn inject_continue(&self) {
        self.inject(Box::new(|_, _| (Next::Continue, ())))
    }

    fn with_ctx<T: 'static + Send>(
        &self,
        f: Box<dyn Fn(FileSpanRef, &mut Evaluator) -> T + Send>,
    ) -> T {
        self.inject(Box::new(move |span, eval| {
            (Next::RemainPaused, f(span, eval))
        }))
    }

    pub fn capabilities() -> Capabilities {
        Capabilities {
            supports_configuration_done_request: Some(true),
            supports_evaluate_for_hovers: Some(true),
            supports_set_variable: Some(true),
            supports_step_in_targets_request: Some(true),
            ..Capabilities::default()
        }
    }

    pub fn set_breakpoints(
        &self,
        x: SetBreakpointsArguments,
    ) -> anyhow::Result<SetBreakpointsResponseBody> {
        let breakpoints = x.breakpoints.unwrap_or_default();
        let source = x.source.path.unwrap();

        if breakpoints.is_empty() {
            self.state.breakpoints.lock().unwrap().remove(&source);
            Ok(SetBreakpointsResponseBody {
                breakpoints: Vec::new(),
            })
        } else {
            match AstModule::parse_file(Path::new(&source), &dialect()) {
                Err(_) => {
                    self.state.breakpoints.lock().unwrap().remove(&source);
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
                    self.state
                        .breakpoints
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

    pub fn stack_trace(&self, _: StackTraceArguments) -> anyhow::Result<StackTraceResponseBody> {
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
        self.with_ctx(Box::new(|span, eval| {
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
        }))
    }

    pub fn scopes(&self, _: ScopesArguments) -> anyhow::Result<ScopesResponseBody> {
        self.with_ctx(Box::new(|_, eval| {
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
        }))
    }

    pub fn variables(&self, _: VariablesArguments) -> anyhow::Result<VariablesResponseBody> {
        self.with_ctx(Box::new(|_, eval| {
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
        }))
    }

    pub fn continue_(&self, _: ContinueArguments) -> anyhow::Result<ContinueResponseBody> {
        self.inject_continue();
        Ok(ContinueResponseBody::default())
    }

    pub fn evaluate(&self, x: EvaluateArguments) -> anyhow::Result<EvaluateResponseBody> {
        let disable_breakpoints = self.state.disable_breakpoints.dupe();
        self.with_ctx(Box::new(move |_, eval| {
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
        }))
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
