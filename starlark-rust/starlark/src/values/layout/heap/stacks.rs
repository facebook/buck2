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

//! Summary of heap allocations and function times with stacks.

use std::cell::RefCell;
use std::collections::hash_map;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::ops::AddAssign;
use std::rc::Rc;
use std::time::Instant;

use either::Either;
use gazebo::dupe::Dupe;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::eval::runtime::profile::flamegraph::FlameGraphWriter;
use crate::eval::runtime::small_duration::SmallDuration;
use crate::values::layout::heap::arena::ArenaVisitor;
use crate::values::layout::heap::heap_type::HeapKind;
use crate::values::layout::heap::repr::AValueOrForward;
use crate::values::layout::heap::stacks::summary::Info;
use crate::values::layout::pointer::RawPointer;
use crate::values::Heap;
use crate::values::Value;

/// Map strings to integers 0, 1, 2, ...
#[derive(Default)]
struct StringIndex {
    strings: SmallSet<String>,
}

impl StringIndex {
    fn index(&mut self, s: &str) -> usize {
        if let Some(index) = self.strings.get_index_of(s) {
            return index;
        }

        let inserted = self.strings.insert(s.to_owned());
        assert!(inserted);
        self.strings.len() - 1
    }

    fn get_all(&self) -> Vec<&str> {
        self.strings.iter().map(|s| s.as_str()).collect()
    }
}

#[derive(Copy, Clone, Dupe, Debug, Eq, PartialEq, Hash)]
pub(crate) struct FunctionId(
    /// Index in strings index.
    pub(crate) usize,
);

/// A mapping from function Value to FunctionId, which must be continuous
#[derive(Default)]
pub(crate) struct FunctionIds {
    values: HashMap<RawPointer, FunctionId>,
    strings: StringIndex,
}

impl FunctionIds {
    pub(crate) fn get_string(&mut self, x: &str) -> FunctionId {
        FunctionId(self.strings.index(x))
    }

    fn get_value(&mut self, x: Value) -> FunctionId {
        match self.values.entry(x.ptr_value()) {
            hash_map::Entry::Occupied(v) => *v.get(),
            hash_map::Entry::Vacant(outer) => {
                let function_id = FunctionId(self.strings.index(&x.to_str()));
                outer.insert(function_id);
                function_id
            }
        }
    }

    pub(crate) fn invert(&self) -> Vec<&str> {
        self.strings.get_all()
    }
}

/// Allocations counters.
#[derive(Default, Copy, Clone, Dupe, Debug)]
pub(crate) struct AllocCounts {
    pub(crate) bytes: usize,
    pub(crate) count: usize,
}

impl AddAssign for AllocCounts {
    fn add_assign(&mut self, other: AllocCounts) {
        self.bytes += other.bytes;
        self.count += other.count;
    }
}

/// A stack frame, its caller and the functions it called, and the allocations it made itself.
struct StackFrameData {
    callees: SmallMap<FunctionId, StackFrameBuilder>,
    allocs: SmallMap<&'static str, AllocCounts>,
    /// Time spent in this frame excluding callees.
    /// Double, because enter/exit are recorded twice, in drop and non-drop heaps.
    time_x2: SmallDuration,
    /// How many times this function was called (with this stack).
    /// Double.
    calls_x2: u32,
}

#[derive(Clone, Dupe)]
struct StackFrameBuilder(pub(crate) Rc<RefCell<StackFrameData>>);

impl StackFrameBuilder {
    fn new() -> Self {
        Self(Rc::new(RefCell::new(StackFrameData {
            callees: Default::default(),
            allocs: Default::default(),
            time_x2: SmallDuration::default(),
            calls_x2: 0,
        })))
    }

    /// Enter a new stack frame.
    fn push(&self, function: FunctionId) -> Self {
        let mut this = self.0.borrow_mut();

        let callee = this
            .callees
            .entry(function)
            .or_insert_with(StackFrameBuilder::new);

        callee.dupe()
    }

    fn build(&self) -> StackFrame {
        StackFrame {
            callees: self
                .0
                .borrow()
                .callees
                .iter()
                .map(|(f, s)| (*f, s.build()))
                .collect(),
            allocs: self.0.borrow().allocs.clone(),
            time_x2: self.0.borrow().time_x2,
            calls_x2: self.0.borrow().calls_x2,
        }
    }
}

/// An accumulator for stack frames that lets us visit the heap.
pub struct StackCollector {
    /// Timestamp of last call enter or exit.
    last_time: Option<Instant>,
    ids: FunctionIds,
    current: Vec<StackFrameBuilder>,
    /// What we are collecting.
    /// When unset, we are collecting allocated memory (not retained).
    /// When set, must be set to correct heap type (unfrozen or frozen), we are traversing.
    retained: Option<HeapKind>,
}

impl StackCollector {
    pub(crate) fn new(retained: Option<HeapKind>) -> Self {
        Self {
            ids: FunctionIds::default(),
            current: vec![StackFrameBuilder::new()],
            last_time: None,
            retained,
        }
    }
}

impl<'v> ArenaVisitor<'v> for StackCollector {
    fn regular_value(&mut self, value: &'v AValueOrForward) {
        let value = match (value.unpack(), self.retained) {
            (Either::Left(header), None) => unsafe { header.unpack_value(HeapKind::Unfrozen) },
            (Either::Right(forward), Some(retained)) => unsafe {
                forward.forward_ptr().unpack_value(retained)
            },
            _ => return,
        };

        let frame = match self.current.last() {
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

    fn call_enter(&mut self, function: Value<'v>, time: Instant) {
        if let Some(last_time) = self.last_time {
            self.current.last_mut().unwrap().0.borrow_mut().time_x2 +=
                time.saturating_duration_since(last_time);
            self.current.last_mut().unwrap().0.borrow_mut().calls_x2 += 1;
        }

        let frame = match self.current.last() {
            Some(frame) => frame,
            None => return,
        };

        // New frame, enter it.
        let id = self.ids.get_value(function);
        let new_frame = frame.push(id);
        self.current.push(new_frame);

        self.last_time = Some(time)
    }

    fn call_exit(&mut self, time: Instant) {
        if let Some(last_time) = self.last_time {
            self.current.last_mut().unwrap().0.borrow_mut().time_x2 +=
                time.saturating_duration_since(last_time);
        }
        self.current.pop().unwrap();
        self.last_time = Some(time);
    }
}

pub(crate) struct StackFrame {
    pub(crate) callees: SmallMap<FunctionId, StackFrame>,
    pub(crate) allocs: SmallMap<&'static str, AllocCounts>,
    pub(crate) time_x2: SmallDuration,
    pub(crate) calls_x2: u32,
}

impl StackFrame {
    /// Write this stack frame's data to a file in a format flamegraph.pl understands
    /// (each line is: `func1:func2:func3 BYTES`).
    fn write<'a>(&self, file: &mut FlameGraphWriter, stack: &'_ mut Vec<&'a str>, ids: &[&'a str]) {
        for (k, v) in &self.allocs {
            file.write(
                stack.iter().copied().chain(std::iter::once(*k)),
                v.bytes as u64,
            );
        }

        for (id, frame) in &self.callees {
            stack.push(ids[id.0]);
            frame.write(file, stack, ids);
            stack.pop();
        }
    }
}

// TODO(nga): rename to `AggregatedProfileInfo`.
pub(crate) struct Stacks {
    pub(crate) ids: FunctionIds,
    pub(crate) root: StackFrame,
    /// String `"TOTALS"`. It is needed in heap summary output.
    pub(crate) totals_id: FunctionId,
    /// String `"(root)"`. It is needed in heap summary output.
    pub(crate) root_id: FunctionId,
    /// String `""`. It is needed in heap summary output.
    pub(crate) blank_id: FunctionId,
}

impl Debug for Stacks {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Stacks").finish_non_exhaustive()
    }
}

impl Stacks {
    pub(crate) fn collect(heap: &Heap, retained: Option<HeapKind>) -> Stacks {
        let mut collector = StackCollector::new(retained);
        unsafe {
            heap.visit_arena(HeapKind::Unfrozen, &mut collector);
        }
        assert_eq!(1, collector.current.len());
        let totals_id = collector.ids.get_string("TOTALS");
        let root_id = collector.ids.get_string("(root)");
        let blank_id = collector.ids.get_string("");
        Stacks {
            ids: collector.ids,
            root: collector.current.pop().unwrap().build(),
            totals_id,
            root_id,
            blank_id,
        }
    }

    /// Write this out recursively to a file.
    pub(crate) fn write(&self) -> String {
        let mut writer = FlameGraphWriter::new();
        self.root
            .write(&mut writer, &mut vec![], &self.ids.invert());
        writer.finish()
    }

    pub(crate) fn gen_summary_csv(&self) -> String {
        Info::init(self).gen_csv(self)
    }
}

mod summary {
    use std::iter;

    use super::*;
    use crate::eval::runtime::profile::csv::CsvWriter;
    use crate::eval::runtime::small_duration::SmallDuration;
    use crate::values::layout::heap::stacks::AllocCounts;
    use crate::values::layout::heap::stacks::FunctionId;
    use crate::values::layout::heap::stacks::StackFrame;

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
        pub alloc: HashMap<&'static str, AllocCounts>,
    }

    impl FuncInfo {
        pub(crate) fn merge<'a>(xs: impl Iterator<Item = &'a Self>) -> Self {
            let mut result = Self::default();
            for x in xs {
                result.calls += x.calls;
                result.time += x.time;
                for (k, v) in x.alloc.iter() {
                    *result.alloc.entry(k).or_default() += *v;
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
        /// Information about all functions
        pub info: Vec<FuncInfo>,
    }

    impl Info {
        pub(crate) fn ensure(&mut self, x: FunctionId) -> &mut FuncInfo {
            if self.info.len() <= x.0 {
                self.info.resize(x.0 + 1, FuncInfo::default());
            }
            &mut self.info[x.0]
        }

        pub(crate) fn init(stacks: &Stacks) -> Info {
            let mut info = Info { info: Vec::new() };
            info.init_children(&stacks.root, stacks.root_id);
            info
        }

        fn init_children(&mut self, frame: &StackFrame, name: FunctionId) -> SmallDuration {
            let mut time_rec = SmallDuration::default();
            for (func, child) in &frame.callees {
                time_rec += self.init_child(*func, child, name);
            }
            time_rec
        }

        fn init_child(
            &mut self,
            func: FunctionId,
            frame: &StackFrame,
            caller: FunctionId,
        ) -> SmallDuration {
            self.ensure(func).time += frame.time_x2;
            self.ensure(func).calls += frame.calls_x2 as usize;
            *self.ensure(func).callers.entry(caller).or_insert(0) += 1;
            for (t, allocs) in &frame.allocs {
                *self.ensure(func).alloc.entry(t).or_default() += *allocs;
            }

            let time_rec = frame.time_x2 + self.init_children(frame, func);
            self.ensure(func).time_rec += time_rec;
            time_rec
        }

        pub(crate) fn gen_csv(&self, stacks: &Stacks) -> String {
            // Add a totals column
            let Info { info } = self;
            let ids = &stacks.ids;
            let totals = FuncInfo::merge(info.iter());
            let mut columns: Vec<(&'static str, AllocCounts)> =
                totals.alloc.iter().map(|(k, v)| (*k, *v)).collect();
            let mut info = info.iter().enumerate().collect::<Vec<_>>();

            columns.sort_by_key(|x| -(x.1.count as isize));
            info.sort_by_key(|x| -(x.1.time.nanos as i128));

            let info = iter::once((stacks.totals_id.0, &totals)).chain(info);

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
            let un_ids = ids.invert();
            for (rowname, info) in info {
                let allocs = info.alloc.values().map(|a| a.count).sum::<usize>();
                let callers = info
                    .callers
                    .iter()
                    .max_by_key(|x| x.1)
                    .unwrap_or((&stacks.blank_id, &0));
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
                    csv.write_value(info.alloc.get(c.0).unwrap_or(&AllocCounts::default()).count);
                }
                csv.finish_row();
            }
            csv.finish()
        }
    }
}

#[cfg(test)]
mod tests {
    use gazebo::dupe::Dupe;

    use crate::const_frozen_string;
    use crate::environment::Globals;
    use crate::environment::Module;
    use crate::eval::Evaluator;
    use crate::eval::ProfileMode;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;
    use crate::values::layout::heap::heap_type::HeapKind;
    use crate::values::layout::heap::stacks::summary::FuncInfo;
    use crate::values::layout::heap::stacks::summary::Info;
    use crate::values::layout::heap::stacks::StackFrame;
    use crate::values::layout::heap::stacks::Stacks;
    use crate::values::Freezer;
    use crate::values::FrozenHeap;
    use crate::values::Heap;

    fn total_alloc_count(frame: &StackFrame) -> usize {
        frame.allocs.values().map(|v| v.count).sum::<usize>()
            + frame
                .callees
                .values()
                .map(|c| total_alloc_count(c.dupe()))
                .sum::<usize>()
    }

    #[test]
    fn test_stacks_collect() {
        let heap = Heap::new();
        heap.record_call_enter(const_frozen_string!("enter").to_value());
        heap.alloc_str("xxyy");
        heap.alloc_str("zzww");
        heap.record_call_exit();

        let stacks = Stacks::collect(&heap, None);
        assert!(stacks.root.allocs.is_empty());
        assert_eq!(1, stacks.root.callees.len());
        assert_eq!(2, total_alloc_count(&stacks.root));
    }

    #[test]
    fn test_stacks_collect_retained() {
        let heap = Heap::new();
        heap.record_call_enter(const_frozen_string!("enter").to_value());
        let s0 = heap.alloc_str("xxyy");
        let s1 = heap.alloc_str("zzww");
        heap.alloc_str("rrtt");
        heap.record_call_exit();

        let freezer = Freezer::new(FrozenHeap::new());
        freezer.freeze(s0.to_value()).unwrap();
        freezer.freeze(s1.to_value()).unwrap();

        let stacks = Stacks::collect(&heap, Some(HeapKind::Frozen));
        assert!(stacks.root.allocs.is_empty());
        assert_eq!(1, stacks.root.callees.len());
        // 3 allocated, 2 retained.
        assert_eq!(
            2,
            stacks
                .root
                .callees
                .values()
                .next()
                .unwrap()
                .allocs
                .get("string")
                .unwrap()
                .count
        );
        assert_eq!(2, total_alloc_count(&stacks.root));
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

        let stacks = Stacks::collect(eval.heap(), None);

        let info = Info::init(&stacks);

        let total = FuncInfo::merge(info.info.iter());
        // from non-drop heap
        assert_eq!(total.alloc.get("string").unwrap().count, 1);
        // from drop heap
        assert_eq!(total.alloc.get("dict").unwrap().count, 1);
    }
}
