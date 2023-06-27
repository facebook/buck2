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

//! Implementation of `struct` function.
use std::collections::BTreeMap;

use allocative::Allocative;
use dupe::Dupe;
use starlark_derive::starlark_module;

use crate as starlark;
use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::environment::GlobalsBuilder;
use crate::eval::Arguments;
use crate::typing::error::TypingError;
use crate::typing::oracle::ctx::TypingOracleCtx;
use crate::typing::ty::TyCustomFunctionImpl;
use crate::typing::ty::TyStruct;
use crate::typing::Arg;
use crate::typing::Ty;
use crate::values::structs::value::FrozenStruct;
use crate::values::structs::value::Struct;
use crate::values::Heap;

#[derive(Allocative, Clone, Copy, Dupe, Debug, Eq, PartialEq, Ord, PartialOrd)]
struct StructType;

impl TyCustomFunctionImpl for StructType {
    fn validate_call(
        &self,
        span: Span,
        args: &[Spanned<Arg>],
        oracle: TypingOracleCtx,
    ) -> Result<Ty, TypingError> {
        let mut fields = BTreeMap::new();
        let mut extra = false;
        for x in args {
            match &x.node {
                Arg::Pos(_) => {
                    return Err(oracle.msg_error(span, "Positional arguments not allowed"));
                }
                Arg::Args(_) => {
                    // Args can be empty, and this is valid call:
                    // ```
                    // struct(*[], **{})
                    // ```
                }
                Arg::Name(name, val) => {
                    fields.insert(name.clone(), val.clone());
                }
                Arg::Kwargs(_) => extra = true,
            }
        }
        Ok(Ty::Struct(TyStruct { fields, extra }))
    }
}

#[starlark_module]
pub fn global(builder: &mut GlobalsBuilder) {
    #[starlark(
        ty_custom_function = StructType,
        as_type = FrozenStruct,
    )]
    fn r#struct<'v>(args: &Arguments<'v, '_>, heap: &'v Heap) -> anyhow::Result<Struct<'v>> {
        args.no_positional_args(heap)?;
        // TODO(nga): missing optimization: practically most `struct` invocations are
        //   performed with fixed named arguments, e.g. `struct(a = 1, b = 2)`.
        //   In this case we can avoid allocating the map, but instead
        //   allocate field index once at compilation time and store field values in a vector.
        Ok(Struct::new(args.names_map()?))
    }
}
