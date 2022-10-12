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

//! Utility to write files in formats understood by `flamegraph.pl`.

use std::borrow::Cow;
use std::fmt::Write;

use starlark_map::small_map::SmallMap;

/// Node in flamegraph tree.
#[derive(Debug, Clone, Default)]
pub(crate) struct FlameGraphNode {
    children: SmallMap<Cow<'static, str>, FlameGraphNode>,
    value: Option<u64>,
}

/// Profiling data as flame tree.
///
/// Can be written to `flamegraph.pl` format.
#[derive(Debug, Clone, Default)]
pub(crate) struct FlameGraphData {
    root: FlameGraphNode,
}

impl FlameGraphNode {
    fn write<'a>(&'a self, writer: &mut FlameGraphWriter, stack: &mut Vec<&'a str>) {
        if let Some(value) = self.value {
            writer.write(stack.iter().copied(), value);
        }
        for (k, v) in self.children.iter() {
            stack.push(k);
            v.write(writer, stack);
            stack.pop().unwrap();
        }
    }

    /// Add value to the node.
    pub(crate) fn add(&mut self, value: u64) {
        match &mut self.value {
            None => self.value = Some(value),
            Some(v) => *v += value,
        }
    }

    /// Get or create a child node.
    pub(crate) fn child(&mut self, name: Cow<'static, str>) -> &mut FlameGraphNode {
        self.children.entry(name).or_default()
    }
}

impl FlameGraphData {
    pub(crate) fn write(&self) -> String {
        let mut writer = FlameGraphWriter::new();
        let mut stack = Vec::new();
        self.root.write(&mut writer, &mut stack);
        assert!(stack.is_empty());
        writer.finish()
    }

    pub(crate) fn root(&mut self) -> &mut FlameGraphNode {
        &mut self.root
    }
}

pub(crate) struct FlameGraphWriter {
    buf: String,
}

impl FlameGraphWriter {
    pub(crate) fn new() -> FlameGraphWriter {
        FlameGraphWriter { buf: String::new() }
    }

    pub(crate) fn write<'s>(&mut self, key: impl IntoIterator<Item = &'s str>, value: u64) {
        let key = key.into_iter().collect::<Vec<_>>();
        if key.is_empty() {
            writeln!(self.buf, "(unknown) {}", value).unwrap();
        } else {
            writeln!(self.buf, "{} {}", key.join(";"), value).unwrap();
        }
    }

    pub(crate) fn finish(self) -> String {
        self.buf
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::runtime::profile::flamegraph::FlameGraphData;
    use crate::eval::runtime::profile::flamegraph::FlameGraphWriter;

    #[test]
    fn test_flamegraph_writer() {
        let mut writer = FlameGraphWriter::new();
        writer.write(["aa", "bb"], 20);
        assert_eq!("aa;bb 20\n", writer.finish());
    }

    #[test]
    fn test_flamegraph_data() {
        let mut data = FlameGraphData::default();
        data.root().child("a".into()).add(10);
        data.root().child("a".into()).child("b".into()).add(20);
        data.root().child("a".into()).add(30);
        let data = data.write();
        assert_eq!("a 40\na;b 20\n", data);
    }
}
