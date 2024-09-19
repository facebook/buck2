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

use crate::eval::ParametersSpec;
use crate::values::FrozenValue;

pub enum NativeSigArg {
    Required(&'static str),
    Optional(&'static str),
    Defaulted(&'static str, FrozenValue),
    Args,
    Kwargs,
}

pub fn parameter_spec(
    name: &'static str,
    num_pos: usize,
    num_pos_only: usize,
    args: &[NativeSigArg],
) -> ParametersSpec<FrozenValue> {
    assert!(
        num_pos_only <= num_pos,
        "Building signature of `{name}`, num_pos_only={num_pos_only} > num_pos={num_pos}"
    );
    assert!(
        num_pos <= args.len(),
        "Building signature of `{name}`, num_pos={num_pos} > args.len()={}",
        args.len()
    );

    let mut spec = ParametersSpec::new(name.to_owned());
    for (i, arg) in args.iter().enumerate() {
        if i == num_pos_only {
            spec.no_more_positional_only_args();
        }
        if i == num_pos && !matches!(arg, NativeSigArg::Args | NativeSigArg::Kwargs) {
            spec.no_more_positional_args();
        }
        match arg {
            NativeSigArg::Required(name) => spec.required(name),
            NativeSigArg::Optional(name) => spec.optional(name),
            NativeSigArg::Defaulted(name, value) => spec.defaulted(name, *value),
            NativeSigArg::Args => spec.args(),
            NativeSigArg::Kwargs => spec.kwargs(),
        }
    }
    spec.finish()
}
