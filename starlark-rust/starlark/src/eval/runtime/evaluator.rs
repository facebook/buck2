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
    cell::Cell,
    intrinsics::unlikely,
    mem::{self, MaybeUninit},
    path::Path,
};

use gazebo::{any::AnyLifetime, cast};
use thiserror::Error;

use crate::{
    codemap::{FileSpan, FileSpanRef},
    collections::{alloca::Alloca, string_pool::StringPool},
    environment::{slots::ModuleSlotId, EnvironmentError, FrozenModuleRef, Module},
    errors::Diagnostic,
    eval::{
        bc::frame::BcFramePtr,
        compiler::def::{Def, DefInfo, FrozenDef},
        runtime::{
            bc_profile::BcProfile,
            before_stmt::BeforeStmt,
            call_stack::{CheapCallStack, FrozenFileSpan},
            flame_profile::FlameProfile,
            heap_profile::{HeapProfile, HeapProfileFormat},
            inlined_frame::InlinedFrames,
            profile::ProfileMode,
            slots::LocalSlotId,
            stmt_profile::StmtProfile,
            typecheck_profile::TypecheckProfile,
        },
        CallStack, FileLoader,
    },
    stdlib::{
        breakpoint::{BreakpointConsole, RealBreakpointConsole},
        extra::{PrintHandler, StderrPrintHandler},
    },
    values::{
        layout::value_captured::{value_captured_get, ValueCaptured},
        FrozenHeap, FrozenRef, Heap, Trace, Tracer, Value, ValueLike,
    },
};

#[derive(Error, Debug)]
pub(crate) enum EvaluatorError {
    #[error("Can't call `write_heap_profile` unless you first call `enable_heap_profile`.")]
    HeapProfilingNotEnabled,
    #[error("Can't call `write_stmt_profile` unless you first call `enable_stmt_profile`.")]
    StmtProfilingNotEnabled,
    #[error("Can't call `write_flame_profile` unless you first call `enable_flame_profile`.")]
    FlameProfilingNotEnabled,
    #[error("Can't call `write_bc_profile` unless you first call `enable_bc_profile`.")]
    BcProfilingNotEnabled,
    #[error("Typecheck profiling not enabled")]
    TypecheckProfilingNotEnabled,
    #[error("No top frame (internal error)")]
    NoTopFrame,
    #[error("Top frame is not def (internal error)")]
    TopFrameNotDef,
}

/// Number of bytes to allocate between GC's.
pub(crate) const GC_THRESHOLD: usize = 100000;

/// Holds everything about an ongoing evaluation (local variables, globals, module resolution etc).
pub struct Evaluator<'v, 'a> {
    // The module that is being used for this evaluation
    pub(crate) module_env: &'v Module,
    // The module-level variables in scope at the moment.
    // If `None` then we're in the initial module, use variables from `module_env`.
    // If `Some` we've called a `def` in a loaded frozen module.
    pub(crate) module_variables: Option<FrozenRef<'static, FrozenModuleRef>>,
    /// Current function (`def` or `lambda`) frame: locals and bytecode stack.
    pub(crate) current_frame: BcFramePtr<'v>,
    // How we deal with a `load` function.
    pub(crate) loader: Option<&'a dyn FileLoader>,
    // `DefInfo` of currently executed function or module.
    pub(crate) def_info: FrozenRef<'static, DefInfo>,
    // Should we enable heap profiling or not
    pub(crate) heap_profile: HeapProfile,
    // Should we enable flame profiling or not
    pub(crate) flame_profile: FlameProfile<'v>,
    // Is either heap or flame profiling enabled
    pub(crate) heap_or_flame_profile: bool,
    // Is GC disabled for some reason
    pub(crate) disable_gc: bool,
    // If true, the interpreter prints to stderr on GC.
    // This is used for debugging.
    pub(crate) verbose_gc: bool,
    // Size of the heap when we should next perform a GC.
    pub(crate) next_gc_level: usize,
    // Extra functions to run on each statement, usually empty
    pub(crate) before_stmt: BeforeStmt<'v, 'a>,
    // Used for line profiling
    stmt_profile: StmtProfile,
    // Bytecode profile.
    pub(crate) bc_profile: BcProfile,
    // Total time spent in runtime typechecking.
    // Filled only if runtime typechecking profiling is enabled.
    pub(crate) typecheck_profile: TypecheckProfile,
    // Used for stack-like allocation
    alloca: Alloca,
    // Another stack-like allocation
    pub(crate) string_pool: StringPool,
    /// Field that can be used for any purpose you want (can store types you define).
    /// Typically accessed via native functions you also define.
    pub extra: Option<&'a dyn AnyLifetime<'a>>,
    /// Field that can be used for any purpose you want (can store heap-resident [`Value<'v>`]).
    /// If this value is used, garbage collection is disabled.
    pub extra_v: Option<&'a dyn AnyLifetime<'v>>,
    /// Called to perform console IO each time `breakpoint` function is called.
    pub(crate) breakpoint_handler: Option<Box<dyn Fn() -> Box<dyn BreakpointConsole>>>,
    /// Use in implementation of `print` function.
    pub(crate) print_handler: &'a (dyn PrintHandler + 'a),
    // The Starlark-level call-stack of functions.
    // Must go last because it's quite a big structure
    pub(crate) call_stack: CheapCallStack<'v>,
}

unsafe impl<'v> Trace<'v> for Evaluator<'v, '_> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        let mut roots = self.module_env.slots().get_slots_mut();
        roots.trace(tracer);
        self.current_frame.trace(tracer);
        self.call_stack.trace(tracer);
        self.flame_profile.trace(tracer);
    }
}

impl<'v, 'a> Evaluator<'v, 'a> {
    /// Crate a new [`Evaluator`] specifying the [`Module`] used for module variables.
    ///
    /// If your program contains `load()` statements, you also need to call
    /// [`set_loader`](Evaluator::set_loader).
    pub fn new(module: &'v Module) -> Self {
        Evaluator {
            call_stack: CheapCallStack::default(),
            module_env: module,
            module_variables: None,
            current_frame: BcFramePtr::null(),
            loader: None,
            extra: None,
            extra_v: None,
            next_gc_level: GC_THRESHOLD,
            disable_gc: false,
            alloca: Alloca::new(),
            heap_profile: HeapProfile::new(),
            stmt_profile: StmtProfile::new(),
            bc_profile: BcProfile::new(),
            typecheck_profile: TypecheckProfile::default(),
            flame_profile: FlameProfile::new(),
            heap_or_flame_profile: false,
            before_stmt: BeforeStmt::default(),
            def_info: DefInfo::empty(), // Will be replaced before it is used
            string_pool: StringPool::default(),
            breakpoint_handler: None,
            print_handler: &StderrPrintHandler,
            verbose_gc: false,
        }
    }

    /// Disables garbage collection from now onwards. Cannot be re-enabled.
    /// Usually called because you have captured [`Value`]'s unsafely, either in
    /// global variables or the [`extra`](Evaluator::extra) field.
    pub fn disable_gc(&mut self) {
        self.disable_gc = true;
    }

    /// Enable GC logging.
    pub fn verbose_gc(&mut self) {
        self.verbose_gc = true;
    }

    /// Set the [`FileLoader`] used to resolve `load()` statements.
    /// A list of all load statements can be obtained through
    /// [`AstModule::loads`](crate::syntax::AstModule::loads).
    pub fn set_loader(&mut self, loader: &'a dyn FileLoader) {
        self.loader = Some(loader);
    }

    /// Enable profiling, allowing [`Evaluator::write_profile`] to be used.
    /// Profilers add overhead, and while some profilers can be used together,
    /// it's better to run at most one profiler at a time.
    pub fn enable_profile(&mut self, mode: &ProfileMode) {
        match mode {
            ProfileMode::HeapSummary | ProfileMode::HeapFlame => {
                self.heap_profile.enable();
                self.heap_or_flame_profile = true;
                // Disable GC because otherwise why lose the profile records, as we use the heap
                // to store a complete list of what happened in linear order.
                self.disable_gc = true;
            }
            ProfileMode::Statement => {
                self.stmt_profile.enable();
                self.before_stmt(&|span, eval| eval.stmt_profile.before_stmt(span));
            }
            ProfileMode::TimeFlame => {
                self.flame_profile.enable();
                self.heap_or_flame_profile = true;
            }
            ProfileMode::Bytecode => {
                self.bc_profile.enable_1();
            }
            ProfileMode::BytecodePairs => {
                self.bc_profile.enable_2();
            }
            ProfileMode::Typecheck => {
                self.typecheck_profile.enabled = true;
            }
        }
    }

    /// Generate instructions to invoke before stmt callbacks when evaluating the module,
    /// even if this module does not use any such callbacks.
    ///
    /// This function need to be called when evaluating a dependency of a module, if a module
    /// registers `before_stmt` callback.
    pub fn enable_before_stmt_instrumentation(&mut self) {
        self.before_stmt.instrument = true;
    }

    /// Enable instrumentation in module which is loaded by a module to be profiled.
    ///
    /// This function need to be called when evaluating a dependency of a module, if a module
    /// does profiling in the given mode.
    pub fn enable_profile_instrumentation(&mut self, mode: &ProfileMode) {
        match mode {
            ProfileMode::Bytecode | ProfileMode::BytecodePairs => {
                self.bc_profile.enable_1();
            }
            _ => {
                self.before_stmt.instrument = true;
            }
        }
    }

    /// Write a profile to a file.
    /// Only valid if corresponding profiler was enabled.
    pub fn write_profile<P: AsRef<Path>>(
        &self,
        mode: &ProfileMode,
        filename: P,
    ) -> anyhow::Result<()> {
        match mode {
            ProfileMode::HeapSummary => self
                .heap_profile
                .write(filename.as_ref(), self.heap(), HeapProfileFormat::Summary)
                .unwrap_or_else(|| Err(EvaluatorError::HeapProfilingNotEnabled.into())),
            ProfileMode::HeapFlame => self
                .heap_profile
                .write(
                    filename.as_ref(),
                    self.heap(),
                    HeapProfileFormat::FlameGraph,
                )
                .unwrap_or_else(|| Err(EvaluatorError::HeapProfilingNotEnabled.into())),
            ProfileMode::Statement => self
                .stmt_profile
                .write(filename.as_ref())
                .unwrap_or_else(|| Err(EvaluatorError::StmtProfilingNotEnabled.into())),
            ProfileMode::Bytecode | ProfileMode::BytecodePairs => {
                self.bc_profile.write_csv(filename.as_ref())
            }
            ProfileMode::TimeFlame => self
                .flame_profile
                .write(filename.as_ref())
                .unwrap_or_else(|| Err(EvaluatorError::FlameProfilingNotEnabled.into())),
            ProfileMode::Typecheck => self
                .typecheck_profile
                .write(filename.as_ref())
                .unwrap_or_else(|| Err(EvaluatorError::TypecheckProfilingNotEnabled.into())),
        }
    }

    /// Enable interactive `breakpoint()`. When enabled, `breakpoint()`
    /// reads commands from stdin and write to stdout.
    /// When disabled (default), `breakpoint()` function results in error.
    pub fn enable_terminal_breakpoint_console(&mut self) {
        self.breakpoint_handler = Some(RealBreakpointConsole::factory());
    }

    /// Obtain the current call-stack, suitable for use with [`Diagnostic`].
    pub fn call_stack(&self) -> CallStack {
        self.call_stack
            .to_diagnostic_frames(InlinedFrames::default())
    }

    /// Obtain the top location on the call-stack. May be [`None`] if the
    /// call happened via native functions.
    pub fn call_stack_top_location(&self) -> Option<FileSpan> {
        self.call_stack.top_location()
    }

    /// Called before every statement is run with the span and a reference to the containing [`Evaluator`].
    /// A list of all possible statements can be obtained in advance by
    /// [`AstModule::stmt_locations`](crate::syntax::AstModule::stmt_locations).
    ///
    /// This function may have no effect is called mid evaluation.
    pub fn before_stmt(&mut self, f: &'a dyn Fn(FileSpanRef, &mut Evaluator<'v, 'a>)) {
        self.before_stmt.before_stmt.push(f)
    }

    /// Set the handler invoked when `print` function is used.
    pub fn set_print_handler(&mut self, handler: &'a (dyn PrintHandler + 'a)) {
        self.print_handler = handler;
    }

    /// Called to add an entry to the call stack, by the function being invoked.
    /// Called for all types of function, including those written in Rust.
    #[inline(always)]
    pub(crate) fn with_call_stack<R>(
        &mut self,
        function: Value<'v>,
        span: Option<FrozenRef<'static, FrozenFileSpan>>,
        within: impl FnOnce(&mut Self) -> anyhow::Result<R>,
    ) -> anyhow::Result<R> {
        #[cold]
        #[inline(never)]
        fn add_diagnostics(e: anyhow::Error, me: &Evaluator) -> anyhow::Error {
            Diagnostic::modify(e, |d: &mut Diagnostic| {
                // Make sure we capture the call_stack before popping things off it
                d.set_call_stack(|| me.call_stack.to_diagnostic_frames(InlinedFrames::default()));
            })
        }

        self.call_stack.push(function, span)?;
        if unlikely(self.heap_or_flame_profile) {
            self.heap_profile.record_call_enter(function, self.heap());
            self.flame_profile.record_call_enter(function);
        }
        // Must always call .pop regardless
        let res = within(self).map_err(|e| add_diagnostics(e, self));
        self.call_stack.pop();
        if unlikely(self.heap_or_flame_profile) {
            self.heap_profile.record_call_exit(self.heap());
            self.flame_profile.record_call_exit();
        }
        res
    }

    /// Called to change the local variables, from the callee.
    /// Only called for user written functions.
    #[inline(always)] // There is only one caller
    pub(crate) fn with_function_context<R>(
        &mut self,
        module: Option<FrozenRef<'static, FrozenModuleRef>>, // None == use module_env
        def_info: FrozenRef<'static, DefInfo>,
        within: impl FnOnce(&mut Self) -> R,
    ) -> R {
        // Capture the variables we will be mutating
        let old_def_info = mem::replace(&mut self.def_info, def_info);

        // Set up for the new function call
        let old_module_variables = mem::replace(&mut self.module_variables, module);

        // Run the computation
        let res = within(self);

        // Restore them all back
        self.def_info = old_def_info;
        self.module_variables = old_module_variables;
        res
    }

    /// The active heap where [`Value`]s are allocated.
    pub fn heap(&self) -> &'v Heap {
        self.module_env.heap()
    }

    /// The frozen heap. It's possible to allocate [`FrozenValue`](crate::values::FrozenValue)s here,
    /// but often not a great idea, as they will remain allocated as long
    /// as the results of this execution are required.
    /// Suitable for use with [`add_reference`](FrozenHeap::add_reference)
    /// and [`OwnedFrozenValue::owned_frozen_value`](crate::values::OwnedFrozenValue::owned_frozen_value).
    pub fn frozen_heap(&self) -> &FrozenHeap {
        self.module_env.frozen_heap()
    }

    pub(crate) fn get_slot_module(&self, slot: ModuleSlotId) -> anyhow::Result<Value<'v>> {
        // Make sure the error-path doesn't get inlined into the normal-path execution
        #[cold]
        #[inline(never)]
        fn error<'v>(eval: &Evaluator<'v, '_>, slot: ModuleSlotId) -> anyhow::Error {
            let name = match &eval.module_variables {
                None => eval.module_env.names().get_slot(slot),
                Some(e) => e.0.get_slot_name(slot),
            }
            .unwrap_or_else(|| "<unknown>".to_owned());
            EnvironmentError::LocalVariableReferencedBeforeAssignment(name).into()
        }

        match &self.module_variables {
            None => self.module_env.slots().get_slot(slot),
            Some(e) => e.0.get_slot(slot).map(Value::new_frozen),
        }
        .ok_or_else(|| error(self, slot))
    }

    // Make sure the error-path doesn't get inlined into the normal-path execution
    #[cold]
    #[inline(never)]
    pub(crate) fn local_var_referenced_before_assignment(
        &self,
        slot: LocalSlotId,
    ) -> anyhow::Error {
        let names = &self.def_info.scope_names.used;
        let name = names[slot.0 as usize].clone();
        EnvironmentError::LocalVariableReferencedBeforeAssignment(name).into()
    }

    #[inline(always)]
    pub(crate) fn get_slot_local(
        &self,
        frame: BcFramePtr<'v>,
        slot: LocalSlotId,
    ) -> anyhow::Result<Value<'v>> {
        // We access locals from explicitly passed frame because it is faster.
        debug_assert!(self.current_frame == frame);

        frame
            .get_slot(slot)
            .ok_or_else(|| self.local_var_referenced_before_assignment(slot))
    }

    pub(crate) fn get_slot_local_captured(&self, slot: LocalSlotId) -> anyhow::Result<Value<'v>> {
        let value_captured = self.get_slot_local(self.current_frame, slot)?;
        let value_captured = value_captured_get(value_captured);
        value_captured.ok_or_else(|| self.local_var_referenced_before_assignment(slot))
    }

    pub(crate) fn clone_slot_capture(&self, slot: LocalSlotId) -> Value<'v> {
        match self.current_frame.get_slot(slot) {
            Some(value_captured) => {
                debug_assert!(
                    value_captured.downcast_ref::<ValueCaptured>().is_some(),
                    "slot {:?} is expected to be ValueCaptured, it is {:?} ({})",
                    slot,
                    value_captured,
                    value_captured.get_type()
                );
                value_captured
            }
            None => {
                let value_captured = self.heap().alloc_complex(ValueCaptured(Cell::new(None)));
                self.current_frame.set_slot(slot, value_captured);
                value_captured
            }
        }
    }

    /// Set a variable in the top-level module currently being processed.
    /// This may not be the module the function is being called in.
    ///
    /// Any variables which are set will be available in the [`Module`] after evaluation returns.
    /// If those variables are _also_ existing top-level variables, then the program from that point on
    /// will incorporate those values. If they aren't existing top-level variables, they will be ignored.
    /// These details are subject to change.
    /// As such, use this API with a healthy dose of caution and in limited settings.
    pub fn set_module_variable_at_some_point(
        &mut self,
        name: &str,
        value: Value<'v>,
    ) -> anyhow::Result<()> {
        value.export_as(name, self);
        self.module_env.set(name, value);
        Ok(())
    }

    pub(crate) fn set_slot_module(&mut self, slot: ModuleSlotId, value: Value<'v>) {
        self.module_env.slots().set_slot(slot, value);
    }

    pub(crate) fn set_slot_local_captured(&mut self, slot: LocalSlotId, value: Value<'v>) {
        match self.current_frame.get_slot(slot) {
            Some(value_captured) => {
                let value_captured = value_captured
                    .downcast_ref::<ValueCaptured>()
                    .expect("not a ValueCaptured");
                value_captured.set(value);
            }
            None => {
                let value_captured = self
                    .heap()
                    .alloc_complex(ValueCaptured(Cell::new(Some(value))));
                self.current_frame.set_slot(slot, value_captured);
            }
        };
    }

    /// Take a value from the local slot and store it back wrapped in [`ValueCaptured`].
    pub(crate) fn wrap_local_slot_captured(&mut self, slot: LocalSlotId) {
        let value = self.current_frame.get_slot(slot).expect("slot unset");
        debug_assert!(value.downcast_ref::<ValueCaptured>().is_none());
        let value_captured = self
            .heap()
            .alloc_complex(ValueCaptured(Cell::new(Some(value))));
        self.current_frame.set_slot(slot, value_captured);
    }

    pub(crate) fn check_return_type(&mut self, ret: Value<'v>) -> anyhow::Result<()> {
        let func = match self.call_stack.top_function() {
            Some(func) => func,
            None => return Err(EvaluatorError::NoTopFrame.into()),
        };
        if let Some(func) = func.downcast_ref::<Def>() {
            func.check_return_type(ret, self)
        } else if let Some(func) = func.downcast_ref::<FrozenDef>() {
            func.check_return_type(ret, self)
        } else {
            Err(EvaluatorError::TopFrameNotDef.into())
        }
    }

    /// Cause a GC to be triggered next time it's possible.
    pub(crate) fn trigger_gc(&mut self) {
        // We will GC next time we can, since the threshold is if 0 or more bytes are allocated
        self.next_gc_level = 0;
    }

    /// Perform a garbage collection.
    /// After this operation all [`Value`]s not reachable from the evaluator will be invalid,
    /// and using them will lead to a segfault.
    /// Do not call during Starlark evaluation.
    pub unsafe fn garbage_collect(&mut self) {
        if self.verbose_gc {
            eprintln!(
                "Starlark: allocated bytes: {}, starting GC...",
                self.heap().allocated_bytes()
            );
        }
        self.heap().garbage_collect(|tracer| self.trace(tracer));
        if self.verbose_gc {
            eprintln!(
                "Starlark: GC complete. Allocated bytes: {}.",
                self.heap().allocated_bytes()
            );
        }
    }

    /// Note that the `Drop` for the `T` will not be called. That's safe if there is no `Drop`,
    /// or you call it yourself.
    #[inline(always)]
    pub(crate) fn alloca_uninit<T, R, F>(&mut self, len: usize, k: F) -> R
    where
        F: FnOnce(&mut [MaybeUninit<T>], &mut Self) -> R,
    {
        // We want to be able to access the evaluator underneath the alloca.
        // We know that the alloca will be used in a stacked way, so that's fine.
        let alloca = unsafe { cast::ptr_lifetime(&self.alloca) };
        alloca.alloca_uninit(len, |xs| k(xs, self))
    }

    /// Allocate `len` elements, initialize them with `init` function, and invoke
    /// a callback `k` with the pointer to the allocated memory and `self`.
    #[inline(always)]
    pub(crate) fn alloca_init<T, R, F>(&mut self, len: usize, init: impl Fn() -> T, k: F) -> R
    where
        F: FnOnce(&mut [T], &mut Self) -> R,
    {
        let alloca = unsafe { cast::ptr_lifetime(&self.alloca) };
        alloca.alloca_init(len, init, |xs| k(xs, self))
    }

    /// Concat two slices and invoke the callback with the result.
    pub(crate) fn alloca_concat<T: Clone, R, F>(&mut self, x: &[T], y: &[T], k: F) -> R
    where
        F: FnOnce(&[T], &mut Self) -> R,
    {
        let alloca = unsafe { cast::ptr_lifetime(&self.alloca) };
        alloca.alloca_concat(x, y, |xs| k(xs, self))
    }
}
