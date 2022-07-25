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

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::Write;
use std::path::Path;
use std::slice;
use std::time::Instant;

use anyhow::Context;
use gazebo::prelude::*;

use crate as starlark;
use crate::eval::runtime::profile::flamegraph::FlameGraphWriter;
use crate::eval::runtime::small_duration::SmallDuration;
use crate::values::layout::pointer::RawPointer;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;

/// Index into FlameData.values
#[derive(Hash, PartialEq, Eq, Clone, Copy, Dupe)]
struct ValueIndex(usize);

impl ValueIndex {
    fn lookup<T>(self, xs: &[T]) -> &T {
        &xs[self.0]
    }
}

enum Frame {
    Push(ValueIndex),
    Pop,
}

#[derive(Trace)]
pub(crate) struct FlameProfile<'v>(Option<Box<FlameData<'v>>>);

/// In order to optimise GC (which otherwise quickly becomes O(n^2)) we have to
/// dedupe the values, so store them in `values`, with a fast map to get them in `map`.
/// Whenever we GC, regenerate map.
#[derive(Default)]
struct FlameData<'v> {
    frames: Vec<(Frame, Instant)>,
    values: Vec<Value<'v>>,
    map: HashMap<RawPointer, ValueIndex>,
}

unsafe impl<'v> Trace<'v> for FlameData<'v> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        self.values.trace(tracer);
        // Have to rebuild the map, as its keyed by ValuePtr which changes on GC
        self.map.clear();
        for (i, x) in self.values.iter().enumerate() {
            self.map.insert(x.ptr_value(), ValueIndex(i));
        }
    }
}

struct Stacks<'a> {
    name: &'a str,
    time: SmallDuration,
    children: HashMap<ValueIndex, Stacks<'a>>,
}

impl<'a> Stacks<'a> {
    fn blank(name: &'a str) -> Self {
        Stacks {
            name,
            time: SmallDuration::default(),
            children: HashMap::new(),
        }
    }

    fn new(names: &'a [String], frames: &[(Frame, Instant)]) -> Self {
        let mut res = Stacks::blank("root");
        let mut last_time = frames.first().map_or_else(Instant::now, |x| x.1);
        res.add(names, &mut frames.iter(), &mut last_time);
        res
    }

    fn add(
        &mut self,
        names: &'a [String],
        frames: &mut slice::Iter<(Frame, Instant)>,
        last_time: &mut Instant,
    ) {
        while let Some((frame, time)) = frames.next() {
            self.time += time.duration_since(*last_time);
            *last_time = *time;
            match frame {
                Frame::Pop => return,
                Frame::Push(i) => match self.children.entry(*i) {
                    Entry::Occupied(mut e) => e.get_mut().add(names, frames, last_time),
                    Entry::Vacant(e) => e
                        .insert(Stacks::blank(i.lookup(names).as_str()))
                        .add(names, frames, last_time),
                },
            }
        }
    }

    fn render_with_buffer(&self, writer: &mut FlameGraphWriter, stack: &mut Vec<&'a str>) {
        stack.push(self.name);
        let count = self.time.to_duration().as_millis();
        if count > 0 {
            writer.write(stack.iter().copied(), count as u64);
        }
        for x in self.children.values() {
            x.render_with_buffer(writer, stack);
        }
        stack.pop().unwrap();
    }

    fn render(&self, mut file: impl Write) -> io::Result<()> {
        let mut stack = Vec::new();
        let mut writer = FlameGraphWriter::new();
        self.render_with_buffer(&mut writer, &mut stack);
        file.write_all(writer.finish().as_bytes())
    }
}

impl<'v> FlameProfile<'v> {
    pub(crate) fn new() -> Self {
        Self(None)
    }

    pub(crate) fn enable(&mut self) {
        self.0 = Some(box FlameData::default());
    }

    #[cold]
    #[inline(never)]
    pub(crate) fn record_call_enter(&mut self, function: Value<'v>) {
        if let Some(box x) = &mut self.0 {
            let ind = match x.map.entry(function.ptr_value()) {
                Entry::Occupied(e) => *e.get(),
                Entry::Vacant(e) => {
                    let res = ValueIndex(x.values.len());
                    x.values.push(function);
                    e.insert(res);
                    res
                }
            };
            x.frames.push((Frame::Push(ind), Instant::now()))
        }
    }

    #[cold]
    #[inline(never)]
    pub(crate) fn record_call_exit(&mut self) {
        if let Some(box x) = &mut self.0 {
            x.frames.push((Frame::Pop, Instant::now()))
        }
    }

    // We could expose profile on the Heap, but it's an implementation detail that it works here.
    pub(crate) fn write(&self, filename: &Path) -> Option<anyhow::Result<()>> {
        self.0
            .as_ref()
            .map(|box x| Self::write_enabled(x, filename))
    }

    fn write_enabled(x: &FlameData, filename: &Path) -> anyhow::Result<()> {
        let file = File::create(filename).with_context(|| {
            format!("When creating profile output file `{}`", filename.display())
        })?;
        Self::write_profile_to(x, file).with_context(|| {
            format!(
                "When writing to profile output file `{}`",
                filename.display()
            )
        })
    }

    fn write_profile_to(x: &FlameData, file: impl Write) -> io::Result<()> {
        // Need to write out lines which look like:
        // root;calls1;calls2 1
        // All the numbers at the end must be whole numbers (we use milliseconds)
        let names = x.values.map(|x| x.to_repr());
        Stacks::new(&names, &x.frames).render(file)
    }
}
