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

#![no_main]

use std::hint::black_box;

use libfuzzer_sys::fuzz_target;
use starlark::environment::Globals;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::syntax::AstModule;
use starlark::syntax::Dialect;

fn run_arbitrary_starlark(content: &str) -> anyhow::Result<()> {
    let ast: AstModule =
        AstModule::parse("hello_world.star", content.to_owned(), &Dialect::Standard)?;
    let globals: Globals = Globals::standard();
    let module: Module = Module::new();
    let mut eval: Evaluator = Evaluator::new(&module);
    let value = black_box(eval.eval_module(ast, &globals)?);
    _ = black_box(format!("{value:?}"));
    Ok(())
}

fuzz_target!(|content: &str| {
    if let Err(e) = black_box(run_arbitrary_starlark(content)) {
        _ = black_box(format!("{e:?}"));
    }
});
