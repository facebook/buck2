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
use std::fs::File;
use std::io::Write;
use std::path::Path;

use anyhow::Context;
use gazebo::dupe::Dupe;

use crate::eval::runtime::profile::csv::CsvWriter;
use crate::values::layout::heap::stacks::AllocCounts;
use crate::values::layout::heap::stacks::FunctionIds;
use crate::values::layout::heap::stacks::Stacks;
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
        let stacks = Stacks::collect(heap, None);
        stacks.write_to(&mut file)?;
        Ok(())
    }

    fn write_summarized_heap_profile_to(mut file: impl Write, heap: &Heap) -> anyhow::Result<()> {
        use summary::FuncInfo;
        use summary::Info;

        let stacks = Stacks::collect(heap, None);
        let mut ids = FunctionIds::default();
        let root = ids.get_string("(root)");
        let mut info = Info {
            stacks,
            info: Vec::new(),
        };
        info.init();
        info.ensure(root);

        // Add a totals column
        let total_id = info.stacks.ids.get_string("TOTALS");
        info.ensure(total_id);
        let Info { mut info, stacks } = info;
        let mut ids = stacks.ids;
        let totals = FuncInfo::merge(info.iter());
        let mut columns: Vec<(&'static str, AllocCounts)> =
            totals.alloc.iter().map(|(k, v)| (*k, *v)).collect();
        info[total_id.0] = totals;
        let mut info = info.iter().enumerate().collect::<Vec<_>>();

        columns.sort_by_key(|x| -(x.1.count as isize));
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
        let blank = ids.get_string("");
        let un_ids = ids.invert();
        for (rowname, info) in info {
            let allocs = info.alloc.values().map(|a| a.count).sum::<usize>();
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
                csv.write_value(info.alloc.get(c.0).unwrap_or(&AllocCounts::default()).count);
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
        pub(crate) stacks: Stacks,

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

        pub(crate) fn init(&mut self) {
            let root = self.stacks.ids.get_string("(root)");
            self.init_children(self.stacks.root.dupe(), root);
        }

        fn init_children(&mut self, frame: StackFrame, name: FunctionId) -> SmallDuration {
            let mut time_rec = SmallDuration::default();
            for (func, child) in &frame.0.borrow().callees {
                time_rec += self.init_child(*func, child.dupe(), name);
            }
            time_rec
        }

        fn init_child(
            &mut self,
            func: FunctionId,
            frame: StackFrame,
            caller: FunctionId,
        ) -> SmallDuration {
            self.ensure(func).time += frame.0.borrow().time_x2;
            self.ensure(func).calls += frame.0.borrow().calls_x2 as usize;
            *self.ensure(func).callers.entry(caller).or_insert(0) += 1;
            for (t, allocs) in &frame.0.borrow().allocs {
                *self.ensure(func).alloc.entry(t).or_default() += *allocs;
            }

            let time_rec = frame.0.borrow().time_x2 + self.init_children(frame.dupe(), func);
            self.ensure(func).time_rec += time_rec;
            time_rec
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

        let stacks = Stacks::collect(eval.heap(), None);

        let mut info = Info {
            stacks,
            info: Vec::new(),
        };
        info.init();

        let total = FuncInfo::merge(info.info.iter());
        // from non-drop heap
        assert_eq!(total.alloc.get("string").unwrap().count, 1);
        // from drop heap
        assert_eq!(total.alloc.get("dict").unwrap().count, 1);
    }
}
