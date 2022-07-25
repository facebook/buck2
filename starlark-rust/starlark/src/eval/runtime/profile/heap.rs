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

use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;
use std::time::Instant;

use anyhow::Context;
use gazebo::prelude::*;

use crate::eval::runtime::profile::csv::CsvWriter;
use crate::eval::runtime::small_duration::SmallDuration;
use crate::values::layout::pointer::RawPointer;
use crate::values::Heap;
use crate::values::Value;

#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) enum HeapProfileFormat {
    Summary,
    FlameGraph,
}

pub(crate) struct HeapProfile {
    enabled: bool,
}

#[derive(Copy, Clone, Dupe, Debug, Eq, PartialEq, Hash)]
struct FunctionId(usize);

/// A mapping from function Value to FunctionId, which must be continuous
#[derive(Default)]
struct FunctionIds {
    values: HashMap<RawPointer, FunctionId>,
    strings: HashMap<String, FunctionId>,
}

impl FunctionIds {
    fn get_string(&mut self, x: String) -> FunctionId {
        let next = FunctionId(self.strings.len());
        match self.strings.entry(x) {
            Entry::Occupied(inner) => *inner.get(),
            Entry::Vacant(inner) => {
                inner.insert(next);
                next
            }
        }
    }

    fn get_value(&mut self, x: Value) -> FunctionId {
        let next = FunctionId(self.strings.len());
        match self.values.entry(x.ptr_value()) {
            Entry::Occupied(v) => *v.get(),
            Entry::Vacant(outer) => {
                let s = x.to_str();
                match self.strings.entry(s) {
                    Entry::Occupied(inner) => {
                        let res = *inner.get();
                        outer.insert(res);
                        res
                    }
                    Entry::Vacant(inner) => {
                        inner.insert(next);
                        outer.insert(next);
                        next
                    }
                }
            }
        }
    }

    fn invert(&self) -> Vec<&str> {
        let mut res = vec![""; self.strings.len()];
        for (name, id) in &self.strings {
            res[id.0] = name.as_str();
        }
        res
    }
}

impl HeapProfile {
    pub(crate) fn new() -> Self {
        Self { enabled: false }
    }

    pub(crate) fn enable(&mut self) {
        self.enabled = true;
    }

    #[cold]
    #[inline(never)]
    pub(crate) fn record_call_enter<'v>(&self, function: Value<'v>, heap: &'v Heap) {
        if self.enabled {
            heap.record_call_enter(function);
        }
    }

    #[cold]
    #[inline(never)]
    pub(crate) fn record_call_exit<'v>(&self, heap: &'v Heap) {
        if self.enabled {
            heap.record_call_exit();
        }
    }

    // We could expose profile on the Heap, but it's an implementation detail that it works here.
    pub(crate) fn write(
        &self,
        filename: &Path,
        heap: &Heap,
        format: HeapProfileFormat,
    ) -> Option<anyhow::Result<()>> {
        if !self.enabled {
            None
        } else {
            Some(Self::write_enabled(filename, heap, format))
        }
    }

    pub(crate) fn write_enabled(
        filename: &Path,
        heap: &Heap,
        format: HeapProfileFormat,
    ) -> anyhow::Result<()> {
        let file = File::create(filename).with_context(|| {
            format!("When creating profile output file `{}`", filename.display())
        })?;

        match format {
            HeapProfileFormat::Summary => Self::write_summarized_heap_profile_to(file, heap),
            HeapProfileFormat::FlameGraph => Self::write_flame_heap_profile_to(file, heap),
        }
        .with_context(|| {
            format!(
                "When writing to profile output file `{}`",
                filename.display()
            )
        })
    }

    fn write_flame_heap_profile_to(mut file: impl Write, heap: &Heap) -> anyhow::Result<()> {
        let mut collector = flame::StackCollector::new();
        unsafe {
            heap.visit_arena(&mut collector);
        }
        collector.write_to(&mut file)?;
        Ok(())
    }

    fn write_summarized_heap_profile_to(mut file: impl Write, heap: &Heap) -> anyhow::Result<()> {
        use summary::FuncInfo;
        use summary::Info;

        let mut ids = FunctionIds::default();
        let root = ids.get_string("(root)".to_owned());
        let start = Instant::now();
        let mut info = Info {
            ids,
            info: Vec::new(),
            last_changed: start,
            call_stack: vec![(root, SmallDuration::default(), start)],
        };
        info.ensure(root);
        unsafe {
            heap.visit_arena(&mut info);
        }
        // Just has root left on it
        assert!(info.call_stack.len() == 1);

        // Add a totals column
        let total_id = info.ids.get_string("TOTALS".to_owned());
        info.ensure(total_id);
        let Info {
            mut info, mut ids, ..
        } = info;
        let totals = FuncInfo::merge(info.iter());
        let mut columns: Vec<(&'static str, usize)> =
            totals.alloc_counts.iter().map(|(k, v)| (*k, *v)).collect();
        info[total_id.0] = totals;
        let mut info = info.iter().enumerate().collect::<Vec<_>>();

        columns.sort_by_key(|x| -(x.1 as isize));
        info.sort_by_key(|x| -(x.1.time.nanos as i128));

        let mut csv = CsvWriter::new(
            [
                "Function",
                "Time(s)",
                "TimeRec(s)",
                "Calls",
                "Callers",
                "TopCaller",
                "TopCallerCount",
                "Allocs",
            ]
            .iter()
            .copied()
            .chain(columns.iter().map(|c| c.0)),
        );
        let blank = ids.get_string("".to_owned());
        let un_ids = ids.invert();
        for (rowname, info) in info {
            let allocs = info.alloc_counts.values().sum::<usize>();
            let callers = info
                .callers
                .iter()
                .max_by_key(|x| x.1)
                .unwrap_or((&blank, &0));
            assert!(
                info.calls % 2 == 0,
                "we enter calls twice, for drop and non_drop"
            );
            // We divide calls and time by two
            // because we could calls twice: for drop and non-drop bumps.
            csv.write_value(un_ids[rowname]);
            csv.write_value(info.time / 2);
            csv.write_value(info.time_rec / 2);
            csv.write_value(info.calls / 2);
            csv.write_value(info.callers.len());
            csv.write_value(un_ids[callers.0.0]);
            csv.write_value(callers.1);
            csv.write_value(allocs);
            for c in &columns {
                csv.write_value(info.alloc_counts.get(c.0).unwrap_or(&0));
            }
            csv.finish_row();
        }
        file.write_all(csv.finish().as_bytes())?;
        Ok(())
    }
}

mod summary {
    use super::*;
    use crate::eval::runtime::small_duration::SmallDuration;
    use crate::values::layout::heap::arena::ArenaVisitor;

    /// Information relating to a function.
    #[derive(Default, Debug, Clone)]
    pub(super) struct FuncInfo {
        /// Number of times this function was called
        pub calls: usize,
        /// Who called this function (and how many times each)
        pub callers: HashMap<FunctionId, usize>,
        /// Time spent directly in this function
        pub time: SmallDuration,
        /// Time spent directly in this function and recursive functions.
        pub time_rec: SmallDuration,
        /// Allocations made by this function
        pub alloc_counts: HashMap<&'static str, usize>,
    }

    impl FuncInfo {
        pub(crate) fn merge<'a>(xs: impl Iterator<Item = &'a Self>) -> Self {
            let mut result = Self::default();
            for x in xs {
                result.calls += x.calls;
                result.time += x.time;
                for (k, v) in x.alloc_counts.iter() {
                    *result.alloc_counts.entry(k).or_insert(0) += v;
                }
            }
            // Recursive time doesn't accumulate nicely, the time is the right value
            result.time_rec = result.time;
            result
        }
    }

    /// We morally have two pieces of information:
    /// 1. Information about each function.
    /// 2. The call stack.
    ///
    /// However, we are always updating the top of the call stack,
    /// so pull out top_stack/top_info as a cache.
    pub(super) struct Info {
        pub ids: FunctionIds,

        /// Information about all functions
        pub info: Vec<FuncInfo>,
        /// When the top of the stack last changed
        pub last_changed: Instant,
        /// Each entry is (Function, time_rec when I started, time I started)
        /// The time_rec is recorded so that recursion doesn't screw up time_rec
        pub call_stack: Vec<(FunctionId, SmallDuration, Instant)>,
    }

    impl Info {
        pub(crate) fn ensure(&mut self, x: FunctionId) {
            if self.info.len() <= x.0 {
                self.info.resize(x.0 + 1, FuncInfo::default());
            }
        }

        pub(crate) fn top_id(&self) -> FunctionId {
            self.call_stack.last().unwrap().0
        }

        pub(crate) fn top_info(&mut self) -> &mut FuncInfo {
            let top = self.top_id();
            &mut self.info[top.0]
        }

        /// Called before you change the top of the stack
        fn change(&mut self, time: Instant) {
            let old_time = self.last_changed;
            let ti = self.top_info();
            ti.time += time.saturating_duration_since(old_time);
            self.last_changed = time;
        }
    }

    impl<'v> ArenaVisitor<'v> for Info {
        fn regular_value(&mut self, x: Value<'v>) {
            let typ = x.get_ref().get_type();
            *self.top_info().alloc_counts.entry(typ).or_insert(0) += 1;
        }

        fn call_enter(&mut self, function: Value<'v>, time: Instant) {
            let id = self.ids.get_value(function);
            self.ensure(id);
            self.change(time);

            let top = self.top_id();
            let mut me = &mut self.info[id.0];
            me.calls += 1;
            *me.callers.entry(top).or_insert(0) += 1;
            self.call_stack.push((id, me.time_rec, time));
        }

        fn call_exit(&mut self, time: Instant) {
            self.change(time);
            let (name, time_rec, start) = self.call_stack.pop().unwrap();
            self.info[name.0].time_rec = time_rec + time.saturating_duration_since(start);
        }
    }
}

mod flame {
    use super::*;
    use crate::eval::runtime::profile::flamegraph::FlameGraphWriter;
    use crate::values::layout::heap::arena::ArenaVisitor;

    /// Allocations made in a given stack frame for a given type.
    #[derive(Default)]
    struct StackFrameAllocations {
        bytes: usize,
        count: usize,
    }

    /// A stack frame, its caller and the functions it called, and the allocations it made itself.
    struct StackFrameData {
        caller: Option<StackFrame>,
        callees: HashMap<FunctionId, StackFrame>,
        allocs: HashMap<&'static str, StackFrameAllocations>,
    }

    #[derive(Clone, Dupe)]
    struct StackFrame(Rc<RefCell<StackFrameData>>);

    impl StackFrame {
        fn new(caller: impl Into<Option<StackFrame>>) -> Self {
            Self(Rc::new(RefCell::new(StackFrameData {
                caller: caller.into(),
                callees: Default::default(),
                allocs: Default::default(),
            })))
        }

        /// Enter a new stack frame.
        fn push(&self, function: FunctionId) -> Self {
            let mut this = self.0.borrow_mut();

            let callee = this
                .callees
                .entry(function)
                .or_insert_with(|| Self::new(self.dupe()));

            callee.dupe()
        }

        /// Exit the last stack frame.
        fn pop(&self) -> Option<Self> {
            let this = self.0.borrow();
            this.caller.as_ref().duped()
        }

        /// Write this stack frame's data to a file in a format flamegraph.pl understands
        /// (each line is: `func1:func2:func3 BYTES`).
        fn write<'a>(
            &self,
            writer: &mut FlameGraphWriter,
            stack: &'_ mut Vec<&'a str>,
            ids: &[&'a str],
        ) {
            let this = self.0.borrow();

            for (k, v) in this.allocs.iter() {
                writer.write(
                    stack.iter().copied().chain(std::iter::once(*k)),
                    v.bytes as u64,
                );
            }

            for (id, frame) in this.callees.iter() {
                stack.push(ids[id.0]);
                frame.write(writer, stack, ids);
                stack.pop();
            }
        }
    }

    /// An accumulator for stack frames that lets us visit the heap.
    pub struct StackCollector {
        ids: FunctionIds,
        current: Option<StackFrame>,
    }

    impl StackCollector {
        pub(crate) fn new() -> Self {
            Self {
                ids: FunctionIds::default(),
                current: Some(StackFrame::new(None)),
            }
        }

        /// Write recursively to a file.
        pub(crate) fn write_to(&self, file: &mut impl Write) -> anyhow::Result<()> {
            let current = self.current.as_ref().context("Popped the root frame")?;
            let mut writer = FlameGraphWriter::new();
            current.write(&mut writer, &mut vec![], &self.ids.invert());
            file.write_all(writer.finish().as_bytes())?;
            Ok(())
        }
    }

    impl<'v> ArenaVisitor<'v> for StackCollector {
        fn regular_value(&mut self, value: Value<'v>) {
            let frame = match self.current.as_ref() {
                Some(frame) => frame,
                None => return,
            };

            // Value allocated in this frame, record it!
            let typ = value.get_ref().get_type();
            let mut frame = frame.0.borrow_mut();
            let mut entry = frame.allocs.entry(typ).or_default();
            entry.bytes += value.get_ref().total_memory();
            entry.count += 1;
        }

        fn call_enter(&mut self, function: Value<'v>, _time: Instant) {
            let frame = match self.current.as_ref() {
                Some(frame) => frame,
                None => return,
            };

            // New frame, enter it.
            let id = self.ids.get_value(function);
            self.current = Some(frame.push(id));
        }

        fn call_exit(&mut self, _time: Instant) {
            let frame = match self.current.as_ref() {
                Some(frame) => frame,
                None => return,
            };

            self.current = frame.pop();
        }
    }

    /// This needs a customized Drop since we have self-referential RCs. If we drop all the
    /// caller fields then that lets the struct get freed.
    impl Drop for StackCollector {
        fn drop(&mut self) {
            fn clear_caller(f: &StackFrame) {
                let mut this = f.0.borrow_mut();
                this.caller = None;

                for callee in this.callees.values() {
                    clear_caller(callee);
                }
            }

            if let Some(frame) = self.current.as_ref() {
                clear_caller(frame)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::environment::Globals;
    use crate::environment::Module;
    use crate::eval::runtime::profile::heap::summary::FuncInfo;
    use crate::eval::runtime::profile::heap::summary::Info;
    use crate::eval::Evaluator;
    use crate::eval::ProfileMode;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;
    use crate::values::Value;

    #[test]
    fn test_profiling() -> anyhow::Result<()> {
        // We don't test that the profile looks any particular way, but we do test it doesn't crash
        let ast = AstModule::parse(
            "foo.bzl",
            r#"
def f(x):
    return (x * 5) + 3
y = 8 * 9 + 2
f
"#
            .to_owned(),
            &Dialect::Extended,
        )?;
        let globals = Globals::standard();
        let module = Module::new();
        let mut eval = Evaluator::new(&module);
        eval.enable_profile(&ProfileMode::HeapSummary);
        let f = eval.eval_module(ast, &globals)?;
        // first check module profiling works
        HeapProfile::write_summarized_heap_profile_to(&mut Vec::new(), module.heap())?;
        HeapProfile::write_flame_heap_profile_to(&mut Vec::new(), module.heap())?;

        // second check function profiling works
        let module = Module::new();
        let mut eval = Evaluator::new(&module);
        eval.enable_profile(&ProfileMode::HeapSummary);
        eval.eval_function(f, &[Value::new_int(100)], &[])?;
        HeapProfile::write_summarized_heap_profile_to(&mut Vec::new(), module.heap())?;
        HeapProfile::write_flame_heap_profile_to(&mut Vec::new(), module.heap())?;

        // finally, check a user can add values into the heap before/after
        let module = Module::new();
        let mut eval = Evaluator::new(&module);
        module.heap().alloc("Thing that goes before");
        eval.enable_profile(&ProfileMode::HeapSummary);
        eval.eval_function(f, &[Value::new_int(100)], &[])?;
        module.heap().alloc("Thing that goes after");
        HeapProfile::write_summarized_heap_profile_to(&mut Vec::new(), module.heap())?;
        HeapProfile::write_flame_heap_profile_to(&mut Vec::new(), module.heap())?;

        Ok(())
    }

    // Test data is collected from both drop and non-drop heaps.
    #[test]
    fn drop_non_drop() {
        let ast = AstModule::parse(
            "x.star",
            "\
_ignore = {1: 2}       # allocate a dict in drop
_ignore = str([1])     # allocate a string in non_drop
        "
            .to_owned(),
            &Dialect::Extended,
        )
        .unwrap();

        let globals = Globals::standard();
        let module = Module::new();
        let mut eval = Evaluator::new(&module);
        eval.enable_profile(&ProfileMode::HeapSummary);

        eval.eval_module(ast, &globals).unwrap();

        let mut ids = FunctionIds::default();
        let root = ids.get_string("(root)".to_owned());
        let mut info = Info {
            ids,
            info: Vec::new(),
            last_changed: Instant::now(),
            call_stack: vec![(root, SmallDuration::default(), Instant::now())],
        };

        unsafe {
            eval.heap().visit_arena(&mut info);
        }

        let total = FuncInfo::merge(info.info.iter());
        // from non-drop heap
        assert_eq!(*total.alloc_counts.get("string").unwrap(), 1);
        // from drop heap
        assert_eq!(*total.alloc_counts.get("dict").unwrap(), 1);
    }
}
