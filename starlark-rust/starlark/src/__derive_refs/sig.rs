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
    NoMorePositionalArgs,
    NoMorePositionalOnlyArgs,
    Args,
    Kwargs,
}

pub fn parameter_spec(name: &'static str, args: &[NativeSigArg]) -> ParametersSpec<FrozenValue> {
    let mut spec = ParametersSpec::new(name.to_owned());
    for arg in args {
        match arg {
            NativeSigArg::Required(name) => spec.required(name),
            NativeSigArg::Optional(name) => spec.optional(name),
            NativeSigArg::Defaulted(name, value) => spec.defaulted(name, *value),
            NativeSigArg::NoMorePositionalArgs => spec.no_more_positional_args(),
            NativeSigArg::NoMorePositionalOnlyArgs => spec.no_more_positional_only_args(),
            NativeSigArg::Args => spec.args(),
            NativeSigArg::Kwargs => spec.kwargs(),
        }
    }
    spec.finish()
}
