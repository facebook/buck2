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

use crate::docs::Doc;
use crate::docs::DocItem;
use crate::environment::Globals;
use crate::stdlib::LibraryExtension;
use crate::typing::basic::TyBasic;
use crate::typing::function::Param;
use crate::typing::oracle::docs::OracleDocs;
use crate::typing::oracle::traits::TypingAttr;
use crate::typing::oracle::traits::TypingBinOp;
use crate::typing::oracle::traits::TypingOracle;
use crate::typing::ty::Ty;
use crate::typing::ty::TyName;
use crate::values::StarlarkValue;

/// A [`TypingOracle`] based on information from documentation.
pub struct OracleStandard {
    /// The things we don't have special code for, but just use whatever docs tells us
    fallback: OracleDocs,
}

impl OracleStandard {
    /// Create a new [`OracleDocs`], following the Starlark standard, but with the given extension you intend to enable.
    pub fn new(extensions: &[LibraryExtension]) -> Self {
        let mut fallback = OracleDocs::new();
        fallback.add_module(&Globals::extended_by(extensions).documentation());

        fn add<T: StarlarkValue<'static>>(fallback: &mut OracleDocs) {
            if let Some(m) = T::get_methods() {
                fallback.add_doc(&Doc::named_item(
                    T::TYPE.to_owned(),
                    DocItem::Object(m.documentation()),
                ));
            }
        }

        add::<crate::values::bool::StarlarkBool>(&mut fallback);
        add::<crate::values::enumeration::FrozenEnumType>(&mut fallback);
        add::<crate::values::float::StarlarkFloat>(&mut fallback);
        add::<crate::values::int::PointerI32>(&mut fallback);
        add::<crate::values::none::NoneType>(&mut fallback);
        add::<crate::values::range::Range>(&mut fallback);
        add::<crate::values::record::field::FrozenField>(&mut fallback);
        add::<crate::values::record::record_type::FrozenRecordType>(&mut fallback);
        add::<crate::values::regex::StarlarkRegex>(&mut fallback);
        add::<crate::values::dict::value::DictGen<crate::values::dict::value::FrozenDictData>>(
            &mut fallback,
        );
        add::<crate::values::list::value::ListGen<crate::values::list::value::FrozenListData>>(
            &mut fallback,
        );
        add::<crate::values::structs::value::FrozenStruct>(&mut fallback);
        add::<crate::values::tuple::value::FrozenTuple>(&mut fallback);

        Self { fallback }
    }
}

impl TypingOracle for OracleStandard {
    fn attribute(&self, ty: &TyBasic, attr: TypingAttr) -> Option<Result<Ty, ()>> {
        let fallback = || match self.fallback.attribute(ty, attr) {
            // We know we have full knowledge, so if we don't know, that must mean the attribute does not exist
            None => Some(Err(())),
            x => x,
        };
        // We have to explicitly implement operators (e.g. `__in__` since we don't generate documentation for them).
        // We explicitly implement polymorphic functions (e.g. `dict.get`) so they can get much more precise types.
        Some(Ok(match ty {
            TyBasic::List(elem) => match attr {
                TypingAttr::Iter => (**elem).clone(),
                TypingAttr::BinOp(TypingBinOp::In) => {
                    Ty::function(vec![Param::pos_only((**elem).clone())], Ty::bool())
                }
                TypingAttr::Index => {
                    Ty::function(vec![Param::pos_only(Ty::int())], (**elem).clone())
                }
                TypingAttr::BinOp(TypingBinOp::Add) => {
                    Ty::function(vec![Param::pos_only(Ty::any())], Ty::list(Ty::any()))
                }
                TypingAttr::BinOp(TypingBinOp::Mul) => {
                    Ty::function(vec![Param::pos_only(Ty::int())], Ty::basic(ty.clone()))
                }
                _ => return fallback(),
            },
            TyBasic::Dict(tk, tv) => match attr {
                TypingAttr::BinOp(TypingBinOp::In) => {
                    Ty::function(vec![Param::pos_only(tk.to_ty())], Ty::bool())
                }
                TypingAttr::BinOp(TypingBinOp::BitOr) => Ty::function(
                    vec![Param::pos_only(Ty::basic(ty.clone()))],
                    Ty::basic(ty.clone()),
                ),
                TypingAttr::Iter => tk.to_ty(),
                TypingAttr::Index => Ty::function(vec![Param::pos_only(tk.to_ty())], tv.to_ty()),
                _ => return fallback(),
            },
            _ => {
                let res = self.fallback.attribute(ty, attr);
                if res.is_none() && self.fallback.known_object(ty.as_name().unwrap_or_default()) {
                    return Some(Err(()));
                } else {
                    return res;
                }
            }
        }))
    }

    fn subtype(&self, _require: &TyName, _got: &TyName) -> bool {
        false
    }
}
