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
    collections::{hash_map::Entry, HashMap},
    fs::File,
    io::Write,
    path::Path,
    time::Instant,
};

use anyhow::Context;
use gazebo::prelude::*;

use crate::{
    codemap::{CodeMap, CodeMapId, FileSpan, FileSpanRef, Span},
    eval::runtime::{profile::csv::CsvWriter, small_duration::SmallDuration},
};

// When line profiling is not enabled, we want this to be small and cheap
pub(crate) struct StmtProfile(Option<Box<StmtProfileData>>);

// So we don't need a special case for the first time around,
// we have a special FileId of empty that we ignore when printing
#[derive(Clone)]
struct StmtProfileData {
    files: HashMap<CodeMapId, CodeMap>,
    stmts: HashMap<(CodeMapId, Span), (usize, SmallDuration)>,
    next_file: CodeMapId,
    last_span: (CodeMapId, Span),
    last_start: Instant,
}

impl StmtProfileData {
    fn new() -> Self {
        StmtProfileData {
            files: HashMap::new(),
            stmts: HashMap::new(),
            next_file: CodeMapId::EMPTY,
            last_span: (CodeMapId::EMPTY, Span::default()),
            last_start: Instant::now(),
        }
    }

    // Add the data from last_span into the entries
    fn add_last(&mut self, now: Instant) {
        let time = now - self.last_start;
        match self.stmts.entry(self.last_span) {
            Entry::Occupied(mut x) => {
                let v = x.get_mut();
                v.0 += 1;
                v.1 += SmallDuration::from_duration(time);
            }
            Entry::Vacant(x) => {
                x.insert((1, SmallDuration::from_duration(time)));
            }
        }
    }

    fn before_stmt(&mut self, span: Span, codemap: &CodeMap) {
        let now = Instant::now();
        self.add_last(now);
        if self.last_span.0 != codemap.id() {
            self.add_codemap(codemap);
        }
        self.last_span = (self.next_file, span);
        self.last_start = now;
    }

    fn add_codemap(&mut self, codemap: &CodeMap) {
        let id = codemap.id();
        self.next_file = id;
        match self.files.entry(id) {
            Entry::Occupied(_) => {
                // Nothing to do, we have already got an owned version of this CodeMap
            }
            Entry::Vacant(x) => {
                x.insert(codemap.dupe());
            }
        }
    }

    fn write(&self, filename: &Path, now: Instant) -> anyhow::Result<()> {
        let file = File::create(filename).with_context(|| {
            format!(
                "When creating line profile output file `{}`",
                filename.display()
            )
        })?;
        self.write_to(file, now).with_context(|| {
            format!(
                "When writing to line profile output file `{}`",
                filename.display()
            )
        })
    }

    fn write_to(&self, mut file: impl Write, now: Instant) -> anyhow::Result<()> {
        // The statement that was running last won't have been properly updated.
        // However, at this point, we have probably run some post-execution code,
        // so it probably wouldn't have a "fair" timing anyway.
        // We do our best though, and give it a time of now.
        // Clone first, since we don't want to impact the real timing with our odd
        // final execution finish.
        let mut data = self.clone();
        data.add_last(now);

        struct Item {
            span: FileSpan,
            time: SmallDuration,
            count: usize,
        }
        // There should be one EMPTY span entry
        let mut items = Vec::with_capacity(data.stmts.len() - 1);
        let mut total_time = SmallDuration::default();
        let mut total_count = 0;
        for ((file, span), (count, time)) in data.stmts {
            // EMPTY represents the first time special-case
            if file != CodeMapId::EMPTY {
                let span = data.files[&file].file_span(span);
                total_time += time;
                total_count += count;
                items.push(Item { span, time, count })
            }
        }
        items.sort_by_key(|x| -(x.time.nanos as i128));

        let mut csv = CsvWriter::new(["File", "Span", "Duration(s)", "Count"]);
        csv.write_value("TOTAL");
        csv.write_value("");
        csv.write_value(total_time);
        csv.write_value(total_count);
        csv.finish_row();

        for x in items {
            csv.write_value(x.span.file.filename());
            csv.write_display(x.span.file.resolve_span(x.span.span));
            csv.write_value(x.time);
            csv.write_value(x.count);
            csv.finish_row();
        }

        file.write_all(csv.finish().as_bytes())?;

        Ok(())
    }
}

impl StmtProfile {
    pub(crate) fn new() -> Self {
        Self(None)
    }

    pub(crate) fn enable(&mut self) {
        self.0 = Some(box StmtProfileData::new())
    }

    pub(crate) fn before_stmt(&mut self, span: FileSpanRef) {
        if let Some(box data) = &mut self.0 {
            data.before_stmt(span.span, span.file)
        }
    }

    // None = not applicable because not enabled
    pub(crate) fn write(&self, filename: &Path) -> Option<anyhow::Result<()>> {
        let now = Instant::now();
        self.0.as_ref().map(|data| data.write(filename, now))
    }
}
