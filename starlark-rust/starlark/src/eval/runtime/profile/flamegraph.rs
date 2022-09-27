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

use std::fmt::Write;

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
    use crate::eval::runtime::profile::flamegraph::FlameGraphWriter;

    #[test]
    fn test_flamegraph_writer() {
        let mut writer = FlameGraphWriter::new();
        writer.write(["aa", "bb"], 20);
        assert_eq!("aa;bb 20\n", writer.finish());
    }
}
