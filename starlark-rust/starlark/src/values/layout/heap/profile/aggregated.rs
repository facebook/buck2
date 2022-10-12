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
use std::collections::hash_map;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::time::Instant;

use either::Either;
use gazebo::dupe::Dupe;
use starlark_map::small_map::SmallMap;

use crate::eval::runtime::profile::flamegraph::FlameGraphData;
use crate::eval::runtime::profile::flamegraph::FlameGraphNode;
use crate::eval::runtime::small_duration::SmallDuration;
use crate::values::layout::heap::arena::ArenaVisitor;
use crate::values::layout::heap::heap_type::HeapKind;
use crate::values::layout::heap::profile::alloc_counts::AllocCounts;
use crate::values::layout::heap::profile::by_type::HeapSummary;
use crate::values::layout::heap::profile::string_index::StringId;
use crate::values::layout::heap::profile::string_index::StringIndex;
use crate::values::layout::heap::profile::summary::HeapSummaryByFunction;
use crate::values::layout::heap::repr::AValueOrForward;
use crate::values::layout::pointer::RawPointer;
use crate::values::Heap;
use crate::values::Value;

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
#[derive(Clone, Default)]
pub(crate) struct StackFrame {
    /// Aggregated callees.
    pub(crate) callees: SmallMap<StringId, StackFrame>,
    /// Aggregated allocations in this frame, without callees.
    pub(crate) allocs: HeapSummary,
    /// Time spend in this frame excluding callees.
    /// `x2` because enter/exit are recorded twice, in drop and non-drop heaps.
    pub(crate) time_x2: SmallDuration,
    /// How many times this frame was called with the same callers.
    /// `x2` because enter/exit are recorded twice, in drop and non-drop heaps.
    pub(crate) calls_x2: u32,
}

impl StackFrame {
    fn merge_callees<'a>(
        frames: &'a [StackFrameWithContext<'a>],
        strings: &mut StringIndex,
    ) -> SmallMap<StringId, StackFrame> {
        let mut group_by_callee: SmallMap<&str, Vec<StackFrameWithContext>> = SmallMap::new();
        for frame in frames {
            for (name, callee) in frame.callees() {
                group_by_callee
                    .entry(name)
                    .or_insert_with(Vec::new)
                    .push(callee);
            }
        }
        group_by_callee
            .into_iter()
            .map(|(name, frames)| {
                let name = strings.index(name);
                (name, StackFrame::merge(frames, strings))
            })
            .collect()
    }

    fn merge<'a>(
        frames: impl IntoIterator<Item = StackFrameWithContext<'a>>,
        strings: &mut StringIndex,
    ) -> StackFrame {
        let frames = Vec::from_iter(frames);
        let callees = StackFrame::merge_callees(&frames, strings);
        let allocs = HeapSummary::merge(frames.iter().map(|f| &f.frame.allocs));
        let time_x2 = frames.iter().map(|f| f.frame.time_x2).sum();
        let calls_x2 = frames.iter().map(|f| f.frame.calls_x2).sum();
        StackFrame {
            callees,
            allocs,
            time_x2,
            calls_x2,
        }
    }
}

struct StackFrameWithContext<'c> {
    frame: &'c StackFrame,
    strings: &'c StringIndex,
}

impl<'c> StackFrameWithContext<'c> {
    fn callees(&self) -> impl Iterator<Item = (&'c str, StackFrameWithContext<'c>)> + '_ {
        self.frame.callees.iter().map(move |(id, callee)| {
            (
                self.strings.get(*id),
                StackFrameWithContext {
                    frame: callee,
                    strings: self.strings,
                },
            )
        })
    }

    /// Write this stack frame's data to a file in flamegraph.pl format.
    fn write_flame_graph(&self, node: &mut FlameGraphNode) {
        for (k, v) in &self.frame.allocs.summary {
            node.child((*k).into()).add(v.bytes as u64);
        }

        for (id, frame) in self.callees() {
            let child_node = node.child(id.to_owned().into());
            frame.write_flame_graph(child_node);
        }
    }
}

/// `Clone` wrapper.
#[derive(Default)]
pub(crate) struct UnusedCapacity(AtomicUsize);

impl Clone for UnusedCapacity {
    fn clone(&self) -> Self {
        UnusedCapacity(AtomicUsize::new(self.0.load(Ordering::Relaxed)))
    }
}

impl UnusedCapacity {
    pub(crate) fn new(value: usize) -> UnusedCapacity {
        UnusedCapacity(AtomicUsize::new(value))
    }

    pub(crate) fn get(&self) -> usize {
        self.0.load(Ordering::Relaxed)
    }

    pub(crate) fn set(&self, value: usize) {
        self.0.store(value, Ordering::Relaxed);
    }
}

/// Aggregated heap profiling data when heap profiling is enabled.
///
/// Can be:
/// * written as CSV or flamegraph
/// * merged with another data
#[derive(Clone)]
pub struct AggregateHeapProfileInfo {
    pub(crate) strings: StringIndex,
    pub(crate) root: StackFrame,
    /// String `"TOTALS"`. It is needed in heap summary output.
    pub(crate) totals_id: StringId,
    /// String `"(root)"`. It is needed in heap summary output.
    pub(crate) root_id: StringId,
    /// String `""`. It is needed in heap summary output.
    pub(crate) blank_id: StringId,
    /// Memory allocated in bump, but unused.
    pub(crate) unused_capacity: UnusedCapacity,
}

impl Debug for AggregateHeapProfileInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("AggregateHeapProfileInfo")
            .finish_non_exhaustive()
    }
}

impl Default for AggregateHeapProfileInfo {
    fn default() -> AggregateHeapProfileInfo {
        let mut strings = StringIndex::default();
        let totals_id = strings.index(AggregateHeapProfileInfo::TOTALS_STR);
        let root_id = strings.index(AggregateHeapProfileInfo::ROOT_STR);
        let blank_id = strings.index(AggregateHeapProfileInfo::BLANK_STR);
        AggregateHeapProfileInfo {
            root: StackFrame::default(),
            strings,
            totals_id,
            root_id,
            blank_id,
            unused_capacity: UnusedCapacity::default(),
        }
    }
}

impl AggregateHeapProfileInfo {
    const TOTALS_STR: &'static str = "TOTALS";
    const ROOT_STR: &'static str = "(root)";
    const BLANK_STR: &'static str = "";

    pub(crate) fn collect(heap: &Heap, retained: Option<HeapKind>) -> AggregateHeapProfileInfo {
        let mut collector = StackCollector::new(retained);
        unsafe {
            heap.visit_arena(HeapKind::Unfrozen, &mut collector);
        }
        assert_eq!(1, collector.current.len());
        let totals_id = collector.ids.strings.index(Self::TOTALS_STR);
        let root_id = collector.ids.strings.index(Self::ROOT_STR);
        let blank_id = collector.ids.strings.index(Self::BLANK_STR);
        AggregateHeapProfileInfo {
            strings: collector.ids.strings,
            root: collector.current.pop().unwrap().build(),
            totals_id,
            root_id,
            blank_id,
            unused_capacity: UnusedCapacity::default(),
        }
    }

    fn root(&self) -> StackFrameWithContext {
        StackFrameWithContext {
            frame: &self.root,
            strings: &self.strings,
        }
    }

    /// Merge aggregated heap profile from multiple sources (e.g. from several runs).
    pub fn merge<'a>(
        profiles: impl IntoIterator<Item = &'a AggregateHeapProfileInfo>,
    ) -> AggregateHeapProfileInfo {
        let profiles: Vec<_> = Vec::from_iter(profiles);

        let mut strings = StringIndex::default();
        let totals_id = strings.index(Self::TOTALS_STR);
        let root_id = strings.index(Self::ROOT_STR);
        let blank_id = strings.index(Self::BLANK_STR);
        let unused_capacity =
            UnusedCapacity::new(profiles.iter().map(|p| p.unused_capacity.get()).sum());
        let roots = profiles.into_iter().map(|p| p.root());
        let root = StackFrame::merge(roots, &mut strings);
        AggregateHeapProfileInfo {
            strings,
            root,
            totals_id,
            root_id,
            blank_id,
            unused_capacity,
        }
    }

    /// Write this out recursively to a file.
    pub fn gen_flame_graph(&self) -> String {
        let mut data = FlameGraphData::default();
        self.root().write_flame_graph(data.root());
        data.write()
    }

    /// Write per-function summary in CSV format.
    pub fn gen_summary_csv(&self) -> String {
        HeapSummaryByFunction::init(self).gen_csv(self)
    }
}

#[cfg(test)]
mod tests {
    use gazebo::dupe::Dupe;

    use crate::const_frozen_string;
    use crate::values::layout::heap::heap_type::HeapKind;
    use crate::values::layout::heap::profile::aggregated::AggregateHeapProfileInfo;
    use crate::values::layout::heap::profile::aggregated::StackFrame;
    use crate::values::layout::heap::profile::summary::HeapSummaryByFunction;
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

    #[test]
    fn test_merge() {
        fn make() -> AggregateHeapProfileInfo {
            let heap = Heap::new();
            heap.record_call_enter(const_frozen_string!("xx").to_value());
            let s = heap.alloc_str("abc");
            heap.record_call_exit();
            let freezer = Freezer::new(FrozenHeap::new());
            freezer.freeze(s.to_value()).unwrap();

            AggregateHeapProfileInfo::collect(&heap, Some(HeapKind::Frozen))
        }

        let merge = AggregateHeapProfileInfo::merge([&make(), &make(), &make()]);
        let summary = HeapSummaryByFunction::init(&merge);
        assert_eq!(1, summary.info().len());
        let (xx_id, xx_info) = summary.info()[0];
        assert_eq!("xx", merge.strings.get(xx_id));
        assert_eq!(3, xx_info.alloc.get("string").unwrap().count);
    }
}
