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

pub(crate) mod alloc_counts;
pub(crate) mod by_type;
mod summary;

use std::cell::RefCell;
use std::collections::hash_map;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
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
use crate::values::layout::heap::profile::alloc_counts::AllocCounts;
use crate::values::layout::heap::profile::by_type::HeapSummary;
use crate::values::layout::heap::profile::summary::HeapSummaryByFunction;
use crate::values::layout::heap::repr::AValueOrForward;
use crate::values::layout::pointer::RawPointer;
use crate::values::Heap;
use crate::values::Value;

/// Map strings to integers 0, 1, 2, ...
#[derive(Default)]
struct StringIndex {
    strings: SmallSet<String>,
}

#[derive(Copy, Clone, Dupe, Debug, Eq, PartialEq, Hash)]
struct StringId(
    /// Index in strings index.
    usize,
);

impl StringIndex {
    fn index(&mut self, s: &str) -> StringId {
        if let Some(index) = self.strings.get_index_of(s) {
            return StringId(index);
        }

        let inserted = self.strings.insert(s.to_owned());
        assert!(inserted);
        StringId(self.strings.len() - 1)
    }

    fn get_all(&self) -> Vec<&str> {
        self.strings.iter().map(|s| s.as_str()).collect()
    }
}

/// A mapping from function Value to FunctionId, which must be continuous
#[derive(Default)]
struct FunctionIds {
    values: HashMap<RawPointer, StringId>,
    strings: StringIndex,
}

impl FunctionIds {
    fn get_value(&mut self, x: Value) -> StringId {
        match self.values.entry(x.ptr_value()) {
            hash_map::Entry::Occupied(v) => *v.get(),
            hash_map::Entry::Vacant(outer) => {
                let function_id = self.strings.index(&x.to_str());
                outer.insert(function_id);
                function_id
            }
        }
    }
}

/// A stack frame, its caller and the functions it called, and the allocations it made itself.
struct StackFrameData {
    callees: SmallMap<StringId, StackFrameBuilder>,
    allocs: HeapSummary,
    /// Time spent in this frame excluding callees.
    /// Double, because enter/exit are recorded twice, in drop and non-drop heaps.
    time_x2: SmallDuration,
    /// How many times this function was called (with this stack).
    /// Double.
    calls_x2: u32,
}

#[derive(Clone, Dupe)]
struct StackFrameBuilder(Rc<RefCell<StackFrameData>>);

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
    fn push(&self, function: StringId) -> Self {
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
pub(crate) struct StackCollector {
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
        frame.allocs.add(
            typ,
            AllocCounts {
                count: 1,
                bytes: value.get_ref().total_memory(),
            },
        );
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

/// Aggregated stack frame data.
struct StackFrame {
    /// Aggregated callees.
    callees: SmallMap<StringId, StackFrame>,
    /// Aggregated allocations in this frame, without callees.
    allocs: HeapSummary,
    /// Time spend in this frame excluding callees.
    /// `x2` because enter/exit are recorded twice, in drop and non-drop heaps.
    time_x2: SmallDuration,
    /// How many times this frame was called with the same callers.
    /// `x2` because enter/exit are recorded twice, in drop and non-drop heaps.
    calls_x2: u32,
}

impl StackFrame {
    /// Write this stack frame's data to a file in flamegraph.pl format.
    fn write_flame_graph<'a>(
        &self,
        file: &mut FlameGraphWriter,
        stack: &'_ mut Vec<&'a str>,
        ids: &[&'a str],
    ) {
        for (k, v) in &self.allocs.summary {
            file.write(
                stack.iter().copied().chain(std::iter::once(*k)),
                v.bytes as u64,
            );
        }

        for (id, frame) in &self.callees {
            stack.push(ids[id.0]);
            frame.write_flame_graph(file, stack, ids);
            stack.pop();
        }
    }
}

pub(crate) struct AggregateHeapProfileInfo {
    strings: StringIndex,
    root: StackFrame,
    /// String `"TOTALS"`. It is needed in heap summary output.
    totals_id: StringId,
    /// String `"(root)"`. It is needed in heap summary output.
    root_id: StringId,
    /// String `""`. It is needed in heap summary output.
    blank_id: StringId,
}

impl Debug for AggregateHeapProfileInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("AggregateHeapProfileInfo")
            .finish_non_exhaustive()
    }
}

impl AggregateHeapProfileInfo {
    pub(crate) fn collect(heap: &Heap, retained: Option<HeapKind>) -> AggregateHeapProfileInfo {
        let mut collector = StackCollector::new(retained);
        unsafe {
            heap.visit_arena(HeapKind::Unfrozen, &mut collector);
        }
        assert_eq!(1, collector.current.len());
        let totals_id = collector.ids.strings.index("TOTALS");
        let root_id = collector.ids.strings.index("(root)");
        let blank_id = collector.ids.strings.index("");
        AggregateHeapProfileInfo {
            strings: collector.ids.strings,
            root: collector.current.pop().unwrap().build(),
            totals_id,
            root_id,
            blank_id,
        }
    }

    /// Write this out recursively to a file.
    pub(crate) fn gen_flame_graph(&self) -> String {
        let mut writer = FlameGraphWriter::new();
        self.root
            .write_flame_graph(&mut writer, &mut vec![], &self.strings.get_all());
        writer.finish()
    }

    pub(crate) fn gen_summary_csv(&self) -> String {
        HeapSummaryByFunction::init(self).gen_csv(self)
    }
}

#[cfg(test)]
mod tests {
    use gazebo::dupe::Dupe;

    use crate::const_frozen_string;
    use crate::values::layout::heap::heap_type::HeapKind;
    use crate::values::layout::heap::profile::AggregateHeapProfileInfo;
    use crate::values::layout::heap::profile::StackFrame;
    use crate::values::Freezer;
    use crate::values::FrozenHeap;
    use crate::values::Heap;

    fn total_alloc_count(frame: &StackFrame) -> usize {
        frame.allocs.total().count
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

        let stacks = AggregateHeapProfileInfo::collect(&heap, None);
        assert!(stacks.root.allocs.summary.is_empty());
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

        let stacks = AggregateHeapProfileInfo::collect(&heap, Some(HeapKind::Frozen));
        assert!(stacks.root.allocs.summary.is_empty());
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
                .summary
                .get("string")
                .unwrap()
                .count
        );
        assert_eq!(2, total_alloc_count(&stacks.root));
    }
}
