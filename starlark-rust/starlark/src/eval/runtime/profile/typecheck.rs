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

use std::time::Duration;

use crate::collections::SmallMap;
use crate::eval::runtime::profile::csv::CsvWriter;
use crate::eval::runtime::profile::data::ProfileData;
use crate::eval::runtime::small_duration::SmallDuration;
use crate::eval::ProfileMode;
use crate::values::FrozenStringValue;

#[derive(Debug, thiserror::Error)]
enum TypecheckProfileErorr {
    #[error("Typecheck profile not enabled")]
    NotEnabled,
}

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
            .or_default() += time;
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
            w.write_display(name.as_str());
            w.write_value(t);
            w.finish_row();
        }

        w.finish()
    }

    pub(crate) fn gen(&self) -> anyhow::Result<ProfileData> {
        if !self.enabled {
            return Err(TypecheckProfileErorr::NotEnabled.into());
        }
        Ok(ProfileData::new(ProfileMode::Typecheck, self.gen_csv()))
    }
}

#[cfg(test)]
mod tests {
    use crate::environment::Globals;
    use crate::environment::Module;
    use crate::eval::Evaluator;
    use crate::eval::ProfileMode;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;

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
        eval.enable_profile(&ProfileMode::Typecheck)?;
        eval.eval_module(program, &Globals::extended())?;

        let csv = eval.typecheck_profile.gen_csv();
        let lines: Vec<&str> = csv.lines().collect();
        assert_eq!("Function,Time (s)", lines[0]);
        assert!(lines[1].starts_with("\"TOTAL\","), "{:?}", lines[1]);
        assert!(lines[2].starts_with("\"f\","), "{:?}", lines[2]);
        assert_eq!(3, lines.len());

        Ok(())
    }
}
