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

use std::cmp::Reverse;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Write;

use dupe::Dupe;
use starlark_syntax::codemap::CodeMaps;

use crate::codemap::CodeMap;
use crate::codemap::CodeMapId;
use crate::codemap::FileSpan;
use crate::codemap::FileSpanRef;
use crate::codemap::ResolvedFileSpan;
use crate::codemap::Span;
use crate::eval::runtime::profile::csv::CsvWriter;
use crate::eval::runtime::profile::data::ProfileData;
use crate::eval::runtime::profile::data::ProfileDataImpl;
use crate::eval::runtime::profile::instant::ProfilerInstant;
use crate::eval::runtime::small_duration::SmallDuration;

#[derive(Debug, thiserror::Error)]
enum StmtProfileError {
    #[error("Statement profiling is not enabled")]
    NotEnabled,
}

// When line profiling is not enabled, we want this to be small and cheap
pub(crate) struct StmtProfile(Option<Box<StmtProfileState>>);

// So we don't need a special case for the first time around,
// we have a special FileId of empty that we ignore when printing
#[derive(Clone)]
struct StmtProfileState {
    files: CodeMaps,
    stmts: HashMap<(CodeMapId, Span), (usize, SmallDuration)>,
    next_file: CodeMapId,
    last_span: (CodeMapId, Span),
    last_start: ProfilerInstant,
}

/// Result of running statement profiler.
#[derive(Clone, Debug)]
pub(crate) struct StmtProfileData {
    files: CodeMaps,
    stmts: HashMap<(CodeMapId, Span), (usize, SmallDuration)>,
}

impl StmtProfileState {
    fn new() -> Self {
        StmtProfileState {
            files: CodeMaps::default(),
            stmts: HashMap::new(),
            next_file: CodeMapId::EMPTY,
            last_span: (CodeMapId::EMPTY, Span::default()),
            last_start: ProfilerInstant::now(),
        }
    }

    // Add the data from last_span into the entries
    fn add_last(&mut self, now: ProfilerInstant) {
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
        let now = ProfilerInstant::now();
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
        self.files.add(codemap);
    }

    fn finish(&self) -> StmtProfileData {
        // The statement that was running last won't have been properly updated.
        // However, at this point, we have probably run some post-execution code,
        // so it probably wouldn't have a "fair" timing anyway.
        // We do our best though, and give it a time of now.
        // Clone first, since we don't want to impact the real timing with our odd
        // final execution finish.
        let now = ProfilerInstant::now();
        let mut data = self.clone();
        data.add_last(now);

        StmtProfileData {
            files: data.files,
            stmts: data.stmts,
        }
    }
}

impl StmtProfileData {
    pub(crate) fn write_to_string(&self) -> String {
        struct Item {
            span: FileSpan,
            time: SmallDuration,
            count: usize,
        }
        // There should be one EMPTY span entry
        let mut items = Vec::with_capacity(self.stmts.len() - 1);
        let mut total_time = SmallDuration::default();
        let mut total_count = 0;
        for ((file, span), &(count, time)) in &self.stmts {
            // EMPTY represents the first time special-case
            if *file != CodeMapId::EMPTY {
                let span = self.files.get(*file).unwrap().file_span(*span);
                total_time += time;
                total_count += count;
                items.push(Item { span, time, count })
            }
        }

        items.sort_by_key(|x| (Reverse(x.time), Reverse(x.count), x.span.dupe()));

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

        csv.finish()
    }

    pub(crate) fn write_coverage(&self) -> String {
        let mut s = String::new();
        let mut keys: Vec<_> = self
            .stmts
            .keys()
            .filter(|(file, _)| *file != CodeMapId::EMPTY)
            .map(|(file, span)| self.files.get(*file).unwrap().file_span(*span).resolve())
            .collect();
        keys.sort();
        for key in keys {
            writeln!(s, "{}", key).unwrap();
        }
        s
    }

    fn coverage(&self) -> HashSet<ResolvedFileSpan> {
        self.stmts
            .keys()
            .filter(|(file, _)| *file != CodeMapId::EMPTY)
            .map(|(code_map_id, span)| {
                self.files
                    .get(*code_map_id)
                    .unwrap()
                    .file_span(*span)
                    .resolve()
            })
            .collect()
    }
}

impl StmtProfile {
    pub(crate) fn new() -> Self {
        Self(None)
    }

    pub(crate) fn enable(&mut self) {
        self.0 = Some(Box::new(StmtProfileState::new()))
    }

    pub(crate) fn before_stmt(&mut self, span: FileSpanRef) {
        if let Some(data) = &mut self.0 {
            data.before_stmt(span.span, span.file)
        }
    }

    // None = not applicable because not enabled
    pub(crate) fn gen(&self) -> crate::Result<ProfileData> {
        match &self.0 {
            Some(data) => Ok(ProfileData {
                profile: ProfileDataImpl::Statement(data.finish()),
            }),
            None => Err(crate::Error::new_other(StmtProfileError::NotEnabled)),
        }
    }

    pub(crate) fn coverage(&self) -> crate::Result<HashSet<ResolvedFileSpan>> {
        Ok(self
            .0
            .as_ref()
            .ok_or_else(|| crate::Error::new_other(StmtProfileError::NotEnabled))?
            .finish()
            .coverage())
    }

    pub(crate) fn gen_coverage(&self) -> crate::Result<ProfileData> {
        match &self.0 {
            Some(data) => Ok(ProfileData {
                profile: ProfileDataImpl::Coverage(data.finish()),
            }),
            None => Err(crate::Error::new_other(StmtProfileError::NotEnabled)),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::assert::test_functions;
    use crate::environment::GlobalsBuilder;
    use crate::environment::Module;
    use crate::eval::runtime::profile::mode::ProfileMode;
    use crate::eval::Evaluator;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;

    #[test]
    fn test_coverage() {
        let module = Module::new();
        let mut eval = Evaluator::new(&module);

        let module = AstModule::parse(
            "cov.star",
            r#"
def xx(x):
    return noop(x)

xx(*[1])
xx(*[2])
"#
            .to_owned(),
            &Dialect::Extended,
        )
        .unwrap();
        eval.enable_profile(&ProfileMode::Coverage).unwrap();
        let mut globals = GlobalsBuilder::standard();
        test_functions(&mut globals);
        eval.eval_module(module, &globals.build()).unwrap();

        let mut coverage: Vec<String> = eval
            .coverage()
            .unwrap()
            .into_iter()
            .map(|s| s.to_string())
            .collect();
        coverage.sort();
        assert_eq!(
            [
                "cov.star:2:1-5:1",
                "cov.star:3:5-19",
                "cov.star:5:1-9",
                "cov.star:6:1-9"
            ]
            .as_slice(),
            coverage
        );
    }
}
