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

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::iter;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use starlark_derive::ProvidesStaticType;

use crate as starlark;
use crate::slice_vec_ext::SliceExt;
use crate::typing::Ty;
use crate::typing::TypingOracleCtx;
use crate::values::tuple::value::Tuple;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::typing::type_compiled::compiled::TypeCompiledBox;
use crate::values::typing::type_compiled::compiled::TypeCompiledImpl;
use crate::values::typing::type_compiled::factory::TypeCompiledFactory;
use crate::values::Value;

#[derive(Eq, PartialEq, Hash, Clone, Dupe, Debug, Ord, PartialOrd, Allocative)]
pub struct TyTuple {
    /// `tuple[T0, T1, T2]`.
    pub(crate) elems: Arc<[Ty]>,
}

impl TyTuple {
    pub(crate) fn get(&self, i: usize) -> Option<&Ty> {
        self.elems.get(i)
    }

    pub(crate) fn item_ty(&self) -> Ty {
        Ty::unions(self.elems.to_vec())
    }

    pub(crate) fn intersects(this: &TyTuple, other: &TyTuple, ctx: &TypingOracleCtx) -> bool {
        this.elems.len() == other.elems.len()
            && iter::zip(&*this.elems, &*other.elems).all(|(x, y)| ctx.intersects(x, y))
    }

    pub(crate) fn matcher<'v>(
        &self,
        type_compiled_factory: TypeCompiledFactory<'v>,
    ) -> TypeCompiled<Value<'v>> {
        #[derive(Eq, PartialEq, Hash, Clone, Allocative, Debug, ProvidesStaticType)]
        struct IsTupleOf(Vec<TypeCompiledBox>);

        impl TypeCompiledImpl for IsTupleOf {
            fn matches(&self, value: Value) -> bool {
                match Tuple::from_value(value) {
                    Some(v) if v.len() == self.0.len() => {
                        v.iter().zip(self.0.iter()).all(|(v, t)| t.0.matches_dyn(v))
                    }
                    _ => false,
                }
            }
        }

        let elems = self
            .elems
            .map(|t| TypeCompiled::from_ty(t, type_compiled_factory.heap()).to_box_dyn());
        type_compiled_factory.alloc(IsTupleOf(elems))
    }
}

impl Display for TyTuple {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &*self.elems {
            [x] => write!(f, "({},)", x),
            xs => display_container::fmt_container(f, "(", ")", xs),
        }
    }
}
