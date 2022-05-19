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

//! Runtime typecheck profile.

use std::{fs, path::Path, time::Duration};

use crate::{
    collections::SmallMap,
    eval::runtime::{csv::CsvWriter, small_duration::SmallDuration},
    values::FrozenStringValue,
};

#[derive(Default, Debug)]
pub(crate) struct TypecheckProfile {
    pub(crate) enabled: bool,
    // TODO(nga): we don't need ordered map here.
    by_function: SmallMap<FrozenStringValue, SmallDuration>,
}

impl TypecheckProfile {
    pub(crate) fn add(&mut self, function: FrozenStringValue, time: Duration) {
        assert!(self.enabled);
        *self
            .by_function
            .entry_hashed(function.get_hashed())
            .or_insert(SmallDuration::default()) += time;
    }

    fn gen_csv(&self) -> String {
        let total_time = self.by_function.values().sum::<SmallDuration>();

        let mut w = CsvWriter::new(["Function", "Time (s)"]);
        w.write_display("TOTAL");
        w.write_value(total_time);
        w.finish_row();

        let mut by_function = Vec::from_iter(&self.by_function);
        by_function.sort_by_key(|(name, t)| (u64::MAX - t.nanos, *name));

        for (name, t) in by_function {
            w.write_display(name);
            w.write_value(t);
            w.finish_row();
        }

        w.finish()
    }

    fn do_write(&self, filename: &Path) -> anyhow::Result<()> {
        fs::write(filename, self.gen_csv())?;
        Ok(())
    }

    pub(crate) fn write(&self, filename: &Path) -> Option<anyhow::Result<()>> {
        if !self.enabled {
            return None;
        }
        Some(self.do_write(filename))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        environment::{Globals, Module},
        eval::{Evaluator, ProfileMode},
        syntax::{AstModule, Dialect},
    };

    #[test]
    fn test_typecheck_profile() -> anyhow::Result<()> {
        let module = Module::new();
        let mut eval = Evaluator::new(&module);
        let program = r#"
def f(s: str.type):
    return int(s)

def g():
    for i in range(0, 1000):
        f(str(i))

g()
"#;
        let program = AstModule::parse("test.star", program.to_owned(), &Dialect::Extended)?;
        eval.enable_profile(&ProfileMode::Typecheck);
        eval.eval_module(program, &Globals::extended())?;

        let csv = eval.typecheck_profile.gen_csv();
        let lines: Vec<&str> = csv.lines().collect();
        assert_eq!("Function,Time (s)", lines[0]);
        assert!(lines[1].starts_with("TOTAL,"));
        assert!(lines[2].starts_with("\"f\","));
        assert_eq!(3, lines.len());

        Ok(())
    }
}
