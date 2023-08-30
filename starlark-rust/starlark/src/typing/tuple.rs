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
use starlark_syntax::slice_vec_ext::SliceExt;

use crate as starlark;
use crate::typing::arc_ty::ArcTy;
use crate::typing::starlark_value::TyStarlarkValue;
use crate::typing::Ty;
use crate::typing::TypingOracleCtx;
use crate::values::tuple::value::Tuple;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::typing::type_compiled::compiled::TypeCompiledBox;
use crate::values::typing::type_compiled::compiled::TypeCompiledImpl;
use crate::values::typing::type_compiled::factory::TypeCompiledFactory;
use crate::values::Value;

#[derive(Eq, PartialEq, Hash, Clone, Dupe, Debug, Ord, PartialOrd, Allocative)]
pub enum TyTuple {
    /// `tuple[T0, T1, T2]`.
    Elems(Arc<[Ty]>),
    /// `tuple[T, ...]`.
    Of(ArcTy),
}

impl TyTuple {
    /// `tuple`.
    pub(crate) fn any() -> TyTuple {
        TyTuple::Of(ArcTy::any())
    }

    pub(crate) fn get(&self, i: usize) -> Option<&Ty> {
        match self {
            TyTuple::Elems(elems) => elems.get(i),
            TyTuple::Of(t) => Some(t),
        }
    }

    pub(crate) fn item_ty(&self) -> Ty {
        match self {
            TyTuple::Elems(elems) => Ty::unions(elems.to_vec()),
            TyTuple::Of(t) => (**t).clone(),
        }
    }

    pub(crate) fn intersects(this: &TyTuple, other: &TyTuple, ctx: &TypingOracleCtx) -> bool {
        match (this, other) {
            (TyTuple::Elems(this), TyTuple::Elems(other)) => {
                this.len() == other.len()
                    && iter::zip(&**this, &**other).all(|(x, y)| ctx.intersects(x, y))
            }
            (TyTuple::Of(this), TyTuple::Of(other)) => ctx.intersects(this, other),
            (TyTuple::Elems(elems), TyTuple::Of(item))
            | (TyTuple::Of(item), TyTuple::Elems(elems)) => {
                // For example `tuple[str, int]` does not intersect with `tuple[str, ...]`.
                elems.iter().all(|x| ctx.intersects(x, item))
            }
        }
    }

    pub(crate) fn matcher<'v>(
        &self,
        type_compiled_factory: TypeCompiledFactory<'v>,
    ) -> TypeCompiled<Value<'v>> {
        match self {
            TyTuple::Elems(elems) => {
                #[derive(Eq, PartialEq, Hash, Clone, Allocative, Debug, ProvidesStaticType)]
                struct Elems(Vec<TypeCompiledBox>);

                impl TypeCompiledImpl for Elems {
                    fn matches(&self, value: Value) -> bool {
                        match Tuple::from_value(value) {
                            Some(v) if v.len() == self.0.len() => {
                                v.iter().zip(self.0.iter()).all(|(v, t)| t.0.matches_dyn(v))
                            }
                            _ => false,
                        }
                    }
                }

                let elems = elems
                    .map(|t| TypeCompiled::from_ty(t, type_compiled_factory.heap()).to_box_dyn());
                type_compiled_factory.alloc(Elems(elems))
            }
            TyTuple::Of(item) if item.is_any() => {
                TyStarlarkValue::new::<Tuple>().type_compiled(type_compiled_factory)
            }
            TyTuple::Of(item) => {
                #[derive(Eq, PartialEq, Hash, Clone, Allocative, Debug, ProvidesStaticType)]
                struct Of(TypeCompiledBox);

                impl TypeCompiledImpl for Of {
                    fn matches(&self, value: Value) -> bool {
                        match Tuple::from_value(value) {
                            Some(v) => v.iter().all(|v| self.0.0.matches_dyn(v)),
                            _ => false,
                        }
                    }
                }

                let item = TypeCompiled::from_ty(item, type_compiled_factory.heap()).to_box_dyn();
                type_compiled_factory.alloc(Of(item))
            }
        }
    }
}

impl Display for TyTuple {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TyTuple::Elems(elems) => match &**elems {
                [x] => write!(f, "({},)", x),
                xs => display_container::fmt_container(f, "(", ")", xs),
            },
            TyTuple::Of(item) => {
                if item.is_any() {
                    write!(f, "tuple")
                } else {
                    write!(f, "tuple[{}, ...]", item)
                }
            }
        }
    }
}
