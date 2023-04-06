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
use std::fmt::Debug;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::mpsc::channel;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;
use std::sync::Arc;
use std::sync::Mutex;

use debugserver_types::*;
use dupe::Dupe;
use gazebo::prelude::SliceExt;

use crate::codemap::FileSpan;
use crate::codemap::FileSpanRef;
use crate::debug::adapter::Breakpoint;
use crate::debug::adapter::ResolvedBreakpoints;
use crate::debug::DapAdapter;
use crate::debug::DapAdapterClient;
use crate::debug::DapAdapterEvalHook;
use crate::debug::ScopesInfo;
use crate::debug::Variable;
use crate::debug::VariablesInfo;
use crate::eval::BeforeStmtFuncDyn;
use crate::eval::Evaluator;
use crate::syntax::AstModule;
use crate::syntax::Dialect;
use crate::values::Value;

pub(crate) fn prepare_dap_adapter(
    client: Box<dyn DapAdapterClient>,
) -> (impl DapAdapter, impl DapAdapterEvalHook) {
    let (sender, receiver) = std::sync::mpsc::channel::<ToEvalMessage>();
    let state = Arc::new(SharedAdapterState {
        client,
        breakpoints: Arc::new(Mutex::new(BreakpointConfig::new())),
        disable_breakpoints: Arc::new(0usize.into()),
    });

    (
        DapAdapterImpl {
            state: state.clone(),
            sender,
        },
        DapAdapterEvalHookImpl::new(state, receiver),
    )
}

type ToEvalMessage = Box<dyn Fn(FileSpanRef, &mut Evaluator) -> Next + Send>;

/// The DapAdapter allows
#[derive(Debug)]
struct DapAdapterImpl {
    state: Arc<SharedAdapterState>,
    sender: Sender<ToEvalMessage>,
}

struct DapAdapterEvalHookImpl {
    state: Arc<SharedAdapterState>,
    receiver: Receiver<ToEvalMessage>,
}

fn evaluate_expr<'v>(
    state: &SharedAdapterState,
    eval: &mut Evaluator<'v, '_>,
    expr: String,
) -> anyhow::Result<Value<'v>> {
    // We don't want to trigger breakpoints during an evaluate,
    // not least because we currently don't allow reenterant evaluate
    state.disable_breakpoints.fetch_add(1, Ordering::SeqCst);
    // Don't use `?`, we need to reset disable_breakpoints.
    let ast = AstModule::parse("interactive", expr, &Dialect::Extended);
    let res = ast.and_then(|ast| eval.eval_statements(ast));
    state.disable_breakpoints.fetch_sub(1, Ordering::SeqCst);
    res
}

impl<'a> BeforeStmtFuncDyn<'a> for DapAdapterEvalHookImpl {
    fn call<'v>(&mut self, span_loc: FileSpanRef, eval: &mut Evaluator<'v, 'a>) {
        let stop = if self.state.disable_breakpoints.load(Ordering::SeqCst) > 0 {
            false
        } else {
            let breaks = self.state.breakpoints.lock().unwrap();
            let breakpoint = breaks.at(span_loc);
            match breakpoint {
                Some(Breakpoint {
                    condition: Some(condition),
                    ..
                }) => match evaluate_expr(&self.state, eval, condition.to_owned()) {
                    Ok(v) => v.to_bool(),
                    _ => true,
                },
                Some(..) => true,
                None => false,
            }
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

impl Debug for DapAdapterEvalHookImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DapAdapterEvaluationWrapper").finish()
    }
}

impl DapAdapterEvalHookImpl {
    fn new(state: Arc<SharedAdapterState>, receiver: Receiver<ToEvalMessage>) -> Self {
        Self { state, receiver }
    }
}

impl DapAdapterEvalHook for DapAdapterEvalHookImpl {
    fn add_dap_hooks<'v, 'a>(self: Box<Self>, eval: &mut Evaluator<'v, 'a>) {
        eval.before_stmt_for_dap((self as Box<dyn BeforeStmtFuncDyn>).into());
    }
}

#[derive(Debug)]
struct BreakpointConfig {
    // maps a source filename to the breakpoint spans for the file
    breakpoints: HashMap<String, HashMap<FileSpan, Breakpoint>>,
}

impl BreakpointConfig {
    fn new() -> Self {
        Self {
            breakpoints: HashMap::new(),
        }
    }

    fn at(&self, span_loc: FileSpanRef) -> Option<&Breakpoint> {
        self.breakpoints
            .get(span_loc.filename())
            .and_then(|file_breaks| file_breaks.get(&span_loc.to_file_span()))
    }

    fn set_breakpoints(
        &mut self,
        source: &str,
        breakpoints: &ResolvedBreakpoints,
    ) -> anyhow::Result<()> {
        if breakpoints.0.is_empty() {
            self.breakpoints.remove(source);
        } else {
            self.breakpoints.insert(
                source.to_owned(),
                breakpoints
                    .0
                    .iter()
                    .filter_map(|x| x.clone())
                    .map(|x| (x.span.dupe(), x))
                    .collect(),
            );
        }
        Ok(())
    }
}

#[derive(Debug)]
struct SharedAdapterState {
    client: Box<dyn DapAdapterClient>,
    // These breakpoints must all match statements as per before_stmt.
    // Those values for which we abort the execution.
    breakpoints: Arc<Mutex<BreakpointConfig>>,
    // Set while we are doing evaluate calls (>= 1 means disable)
    disable_breakpoints: Arc<AtomicUsize>,
}

enum Next {
    Continue,
    RemainPaused,
}

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

impl DapAdapter for DapAdapterImpl {
    fn set_breakpoints(
        &self,
        source: &str,
        breakpoints: &ResolvedBreakpoints,
    ) -> anyhow::Result<()> {
        self.state
            .breakpoints
            .lock()
            .unwrap()
            .set_breakpoints(source, breakpoints)
    }

    fn top_frame(&self) -> anyhow::Result<Option<StackFrame>> {
        self.with_ctx(Box::new(|span, eval| {
            let frame = eval.call_stack_top_frame();
            let name = frame.map_or("".to_owned(), |v| v.name);
            Ok(Some(convert_frame(0, name, Some(span.to_file_span()))))
        }))
    }

    fn stack_trace(&self, _: StackTraceArguments) -> anyhow::Result<StackTraceResponseBody> {
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

    fn scopes(&self) -> anyhow::Result<ScopesInfo> {
        self.with_ctx(Box::new(|_, eval| {
            let vars = eval.local_variables();
            Ok(ScopesInfo {
                num_locals: vars.len(),
            })
        }))
    }

    fn variables(&self) -> anyhow::Result<VariablesInfo> {
        self.with_ctx(Box::new(|_, eval| {
            let vars = eval.local_variables();
            Ok(VariablesInfo {
                locals: vars
                    .into_iter()
                    .map(|(name, value)| Variable {
                        name,
                        value: value.to_string(),
                        type_: value.get_type().to_owned(),
                    })
                    .collect(),
            })
        }))
    }

    fn continue_(&self, _: ContinueArguments) -> anyhow::Result<ContinueResponseBody> {
        self.inject_continue();
        Ok(ContinueResponseBody::default())
    }

    fn evaluate(&self, expr: &str) -> anyhow::Result<EvaluateResponseBody> {
        let state = self.state.dupe();
        let expression = expr.to_owned();
        self.with_ctx(Box::new(move |_, eval| {
            let s = match evaluate_expr(&state, eval, expression.clone()) {
                Err(e) => format!("{:#}", e),
                Ok(v) => v.to_string(),
            };
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

impl DapAdapterImpl {
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
}

pub(crate) fn breakpoint(verified: bool) -> debugserver_types::Breakpoint {
    debugserver_types::Breakpoint {
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

pub(crate) fn resolve_breakpoints(
    args: &SetBreakpointsArguments,
    ast: &AstModule,
) -> anyhow::Result<ResolvedBreakpoints> {
    let poss: HashMap<usize, FileSpan> = ast
        .stmt_locations()
        .iter()
        .map(|span| (span.resolve_span().begin_line, span.dupe()))
        .collect();
    Ok(ResolvedBreakpoints(args.breakpoints.as_ref().map_or(
        Vec::new(),
        |v| {
            v.map(|x| {
                poss.get(&(x.line as usize - 1)).map(|span| Breakpoint {
                    span: span.clone(),
                    condition: x.condition.clone(),
                })
            })
        },
    )))
}

pub(crate) fn resolved_breakpoints_to_dap(
    breakpoints: &ResolvedBreakpoints,
) -> SetBreakpointsResponseBody {
    SetBreakpointsResponseBody {
        breakpoints: breakpoints.0.map(|x| breakpoint(x.is_some())),
    }
}
