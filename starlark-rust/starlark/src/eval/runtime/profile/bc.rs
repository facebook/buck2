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

//! Bytecode profiler.

use std::collections::HashMap;
use std::fs;
use std::iter::Sum;
use std::path::Path;

use gazebo::prelude::*;

use crate::eval::bc::opcode::BcOpcode;
use crate::eval::runtime::evaluator::EvaluatorError;
use crate::eval::runtime::profile::csv::CsvWriter;

#[derive(Default, Clone, Dupe, Copy)]
struct BcInstrStat {
    count: u64,
}

impl<'a> Sum<&'a BcInstrStat> for BcInstrStat {
    fn sum<I: Iterator<Item = &'a BcInstrStat>>(iter: I) -> Self {
        let mut sum = BcInstrStat::default();
        for BcInstrStat { count } in iter {
            sum.count += *count;
        }
        sum
    }
}

#[derive(Default, Clone, Copy, Dupe)]
struct BcInstrPairsStat {
    count: u64,
    // We are not measuring time here, because even time for single opcode
    // is not very accurate or helpful, and time for pairs is even less helpful.
}

struct BcProfileData {
    by_instr: [BcInstrStat; BcOpcode::COUNT],
}

#[derive(Default)]
struct BcPairsProfileData {
    last: Option<BcOpcode>,
    by_instr: HashMap<[BcOpcode; 2], BcInstrPairsStat>,
}

// Derive doesn't work here.
impl Default for BcProfileData {
    fn default() -> Self {
        BcProfileData {
            by_instr: [BcInstrStat::default(); BcOpcode::COUNT],
        }
    }
}

impl BcProfileData {
    fn before_instr(&mut self, opcode: BcOpcode) {
        self.by_instr[opcode as usize].count += 1;
    }

    fn gen_csv(&self) -> String {
        let mut by_instr: Vec<_> = self
            .by_instr
            .iter()
            .enumerate()
            .map(|(i, st)| (BcOpcode::by_number(i as u32).unwrap(), st))
            .collect();
        by_instr.sort_by_key(|(_opcode, st)| u64::MAX - st.count);
        let total: BcInstrStat = by_instr.iter().map(|(_opcode, st)| *st).sum();
        let mut csv = CsvWriter::new(["Opcode", "Count", "Count / Total"]);
        {
            csv.write_display("TOTAL");
            csv.write_value(total.count);
            csv.write_display(format!("{:.3}", 1.0));
            csv.finish_row();
        }
        for (opcode, instr_stats) in &by_instr {
            csv.write_debug(opcode);
            csv.write_value(instr_stats.count);
            csv.write_display(format!(
                "{:.3}",
                instr_stats.count as f64 / total.count as f64
            ));
            csv.finish_row();
        }
        csv.finish()
    }
}

impl BcPairsProfileData {
    fn before_instr(&mut self, opcode: BcOpcode) {
        if let Some(last_opcode) = self.last {
            self.by_instr
                .entry([last_opcode, opcode])
                .or_default()
                .count += 1;
        }
        self.last = Some(opcode);
    }

    fn gen_csv(&self) -> String {
        let mut by_instr: Vec<_> = self
            .by_instr
            .iter()
            .map(|(opcodes, stat)| (*opcodes, stat))
            .collect();
        by_instr.sort_by_key(|(opcodes, st)| (u64::MAX - st.count, *opcodes));
        let count_total = by_instr.iter().map(|(_, st)| st.count).sum::<u64>();
        let mut csv = CsvWriter::new(["Opcode[0]", "Opcode[1]", "Count", "Count / Total"]);
        for ([o0, o1], instr_stats) in &by_instr {
            csv.write_debug(o0);
            csv.write_debug(o1);
            csv.write_value(instr_stats.count);
            csv.write_display(format!(
                "{:.3}",
                instr_stats.count as f64 / count_total as f64
            ));
            csv.finish_row();
        }
        csv.finish()
    }
}

enum BcProfileDataMode {
    Bc(Box<BcProfileData>),
    BcPairs(Box<BcPairsProfileData>),
    Disabled,
}

pub(crate) struct BcProfile {
    data: BcProfileDataMode,
}

impl BcProfile {
    pub(crate) fn new() -> BcProfile {
        BcProfile {
            data: BcProfileDataMode::Disabled,
        }
    }

    pub(crate) fn enable_1(&mut self) {
        self.data = BcProfileDataMode::Bc(Default::default());
    }

    pub(crate) fn enable_2(&mut self) {
        self.data = BcProfileDataMode::BcPairs(Default::default());
    }

    pub(crate) fn enabled(&self) -> bool {
        match self.data {
            BcProfileDataMode::Bc(..) => true,
            BcProfileDataMode::BcPairs(..) => true,
            BcProfileDataMode::Disabled => false,
        }
    }

    fn gen_csv(&self) -> anyhow::Result<String> {
        match &self.data {
            BcProfileDataMode::Bc(data) => Ok(data.gen_csv()),
            BcProfileDataMode::BcPairs(data) => Ok(data.gen_csv()),
            BcProfileDataMode::Disabled => Err(EvaluatorError::BcProfilingNotEnabled.into()),
        }
    }

    pub(crate) fn write_csv(&self, path: &Path) -> anyhow::Result<()> {
        fs::write(path, self.gen_csv()?.as_bytes())?;
        Ok(())
    }

    /// Called from bytecode.
    pub(crate) fn before_instr(&mut self, opcode: BcOpcode) {
        match &mut self.data {
            BcProfileDataMode::Bc(data) => data.before_instr(opcode),
            BcProfileDataMode::BcPairs(data) => data.before_instr(opcode),
            BcProfileDataMode::Disabled => {
                unreachable!("this code is unreachable when bytecode profiling is not enabled")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::environment::Globals;
    use crate::environment::Module;
    use crate::eval::bc::opcode::BcOpcode;
    use crate::eval::Evaluator;
    use crate::eval::ProfileMode;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;

    #[test]
    fn test_smoke() {
        let module = Module::new();
        let globals = Globals::standard();
        let mut eval = Evaluator::new(&module);
        eval.enable_profile(&ProfileMode::Bytecode);
        eval.eval_module(
            AstModule::parse("bc.star", "repr([1, 2])".to_owned(), &Dialect::Standard).unwrap(),
            &globals,
        )
        .unwrap();
        let csv = eval.bc_profile.gen_csv().unwrap();
        assert!(
            csv.contains(&format!("\n\"{:?}\",1,", BcOpcode::CallFrozenNativePos)),
            "{:?}",
            csv
        );
    }

    #[test]
    fn test_smoke_2() {
        let module = Module::new();
        let globals = Globals::standard();
        let mut eval = Evaluator::new(&module);
        eval.enable_profile(&ProfileMode::BytecodePairs);
        eval.eval_module(
            AstModule::parse("bc.star", "repr([1, 2])".to_owned(), &Dialect::Standard).unwrap(),
            &globals,
        )
        .unwrap();
        let csv = eval.bc_profile.gen_csv().unwrap();
        assert!(
            csv.contains(&format!(
                "\n\"{:?}\",\"{:?}\",1",
                BcOpcode::ListOfConsts,
                BcOpcode::CallFrozenNativePos
            )),
            "{:?}",
            csv
        );
    }
}
