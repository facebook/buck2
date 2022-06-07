/*
 * Copyright 2018 The Starlark in Rust Authors.
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

use std::{env, fmt::Write, fs};

use anyhow::Context;

use crate::{assert::Assert, eval::compiler::def::FrozenDef};

const REGENERATE_VAR_NAME: &str = "STARLARK_RUST_REGENERATE_BC_TESTS";

#[allow(clippy::write_literal)] // We mark generated files as generated, but not this file.
fn make_golden(program: &str) -> String {
    let program = program.trim();

    let mut a = Assert::new();
    let def = a
        .module("instrs.star", program)
        .get("test")
        .unwrap()
        .downcast::<FrozenDef>()
        .unwrap();

    let mut golden = String::new();
    writeln!(golden, "# {at}generated", at = "@").unwrap();
    writeln!(golden, "# To regenerate, run:").unwrap();
    writeln!(golden, "# ```").unwrap();
    writeln!(
        golden,
        "# {REGENERATE_VAR_NAME}=1 cargo test -p starlark --lib tests"
    )
    .unwrap();
    writeln!(golden, "# ```").unwrap();
    writeln!(golden).unwrap();
    writeln!(golden, "{}", program).unwrap();
    writeln!(golden).unwrap();
    writeln!(golden, "# Bytecode:").unwrap();
    writeln!(golden).unwrap();
    writeln!(golden, "{}", def.bc().dump_debug().trim()).unwrap();
    golden
}

pub(crate) fn bc_golden_test(test_name: &str, program: &str) {
    let manifest_dir =
        env::var("CARGO_MANIFEST_DIR").expect("`CARGO_MANIFEST_DIR` variable must be set");

    let golden_file_name = format!("{manifest_dir}/src/tests/bc/golden/{test_name}.golden");

    let actual = make_golden(program);
    if env::var(REGENERATE_VAR_NAME).is_ok() {
        fs::write(golden_file_name, &actual).unwrap();
    } else {
        let expected = fs::read_to_string(&golden_file_name)
            .with_context(|| format!("Reading `{golden_file_name}`"))
            .unwrap();
        assert_eq!(expected, actual);
    }
}
