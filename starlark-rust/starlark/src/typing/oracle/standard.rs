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
    fn attribute(&self, ty: &Ty, attr: TypingAttr) -> Option<Result<Ty, ()>> {
        let fallback = || match self.fallback.attribute(ty, attr) {
            // We know we have full knowledge, so if we don't know, that must mean the attribute does not exist
            None => Some(Err(())),
            x => x,
        };
        // We have to explicitly implement operators (e.g. `__in__` since we don't generate documentation for them).
        // We explicitly implement polymorphic functions (e.g. `dict.get`) so they can get much more precise types.
        Some(Ok(match ty {
            Ty::List(elem) => match attr {
                TypingAttr::Slice => ty.clone(),
                TypingAttr::BinOp(TypingBinOp::Less) => {
                    // This is a bit weak, beacuse it only looks at this oracle
                    return self.attribute(ty, attr);
                }

                TypingAttr::Iter => (**elem).clone(),
                TypingAttr::BinOp(TypingBinOp::In) => {
                    Ty::function(vec![Param::pos_only((**elem).clone())], Ty::bool())
                }
                TypingAttr::Index => {
                    Ty::function(vec![Param::pos_only(Ty::int())], (**elem).clone())
                }
                TypingAttr::BinOp(TypingBinOp::Add) => {
                    Ty::function(vec![Param::pos_only(Ty::Any)], Ty::list(Ty::Any))
                }
                TypingAttr::BinOp(TypingBinOp::Mul) => {
                    Ty::function(vec![Param::pos_only(Ty::int())], ty.clone())
                }
                TypingAttr::Regular("pop") => Ty::function(
                    vec![Param::pos_only(Ty::int()).optional()],
                    (**elem).clone(),
                ),
                TypingAttr::Regular("index") => Ty::function(
                    vec![
                        Param::pos_only((**elem).clone()),
                        Param::pos_only(Ty::int()).optional(),
                    ],
                    Ty::int(),
                ),
                TypingAttr::Regular("remove") => {
                    Ty::function(vec![Param::pos_only((**elem).clone())], Ty::none())
                }
                _ => return fallback(),
            },
            Ty::Dict(tk_tv) => {
                let (ref tk, ref tv) = **tk_tv;
                match attr {
                    TypingAttr::BinOp(TypingBinOp::In) => {
                        Ty::function(vec![Param::pos_only(tk.clone())], Ty::bool())
                    }
                    TypingAttr::BinOp(TypingBinOp::BitOr) => {
                        Ty::function(vec![Param::pos_only(ty.clone())], ty.clone())
                    }
                    TypingAttr::Iter => tk.clone(),
                    TypingAttr::Index => {
                        Ty::function(vec![Param::pos_only(tk.clone())], tv.clone())
                    }
                    TypingAttr::Regular("get") => Ty::union2(
                        Ty::function(
                            vec![Param::pos_only(tk.clone())],
                            Ty::union2(tv.clone(), Ty::none()),
                        ),
                        // This second signature is a bit too lax, but get with a default is much rarer
                        Ty::function(
                            vec![Param::pos_only(tk.clone()), Param::pos_only(Ty::Any)],
                            Ty::Any,
                        ),
                    ),
                    TypingAttr::Regular("keys") => Ty::function(vec![], Ty::list(tk.clone())),
                    TypingAttr::Regular("values") => Ty::function(vec![], Ty::list(tv.clone())),
                    TypingAttr::Regular("items") => {
                        Ty::function(vec![], Ty::list(Ty::Tuple(vec![tk.clone(), tv.clone()])))
                    }
                    TypingAttr::Regular("popitem") => {
                        Ty::function(vec![], Ty::Tuple(vec![tk.clone(), tv.clone()]))
                    }
                    _ => return fallback(),
                }
            }
            Ty::StarlarkValue(x) if x.as_name() == "int" => match attr {
                TypingAttr::BinOp(TypingBinOp::Less) => {
                    Ty::function(vec![Param::pos_only(Ty::int())], Ty::bool())
                }
                TypingAttr::BinOp(TypingBinOp::Sub) => {
                    Ty::function(vec![Param::pos_only(Ty::int())], Ty::int())
                }
                TypingAttr::BinOp(TypingBinOp::Add) => {
                    Ty::function(vec![Param::pos_only(Ty::int())], Ty::int())
                }
                TypingAttr::UnOp(un_op) => match x.un_op(un_op) {
                    Ok(res) => Ty::function(vec![], Ty::StarlarkValue(res)),
                    Err(()) => return Some(Err(())),
                },
                TypingAttr::BinOp(TypingBinOp::Div) => {
                    Ty::function(vec![Param::pos_only(Ty::Any)], Ty::float())
                }
                _ => return Some(Err(())),
            },
            Ty::StarlarkValue(x) if x.as_name() == "float" => match attr {
                TypingAttr::BinOp(TypingBinOp::Less) => {
                    Ty::function(vec![Param::pos_only(Ty::float())], Ty::bool())
                }
                TypingAttr::BinOp(TypingBinOp::Sub) => {
                    Ty::function(vec![Param::pos_only(Ty::float())], Ty::float())
                }
                TypingAttr::BinOp(TypingBinOp::Add) => {
                    Ty::function(vec![Param::pos_only(Ty::float())], Ty::float())
                }
                TypingAttr::UnOp(un_op) => match x.un_op(un_op) {
                    Ok(res) => Ty::function(vec![], Ty::StarlarkValue(res)),
                    Err(()) => return Some(Err(())),
                },
                TypingAttr::BinOp(TypingBinOp::Div) => {
                    Ty::function(vec![Param::pos_only(Ty::Any)], Ty::float())
                }
                _ => return Some(Err(())),
            },
            Ty::StarlarkValue(x) if x.as_name() == "string" => match attr {
                TypingAttr::BinOp(TypingBinOp::Less) => {
                    Ty::function(vec![Param::pos_only(Ty::string())], Ty::bool())
                }
                TypingAttr::Index => Ty::function(vec![Param::pos_only(Ty::int())], Ty::string()),
                TypingAttr::BinOp(TypingBinOp::In) => {
                    Ty::function(vec![Param::pos_only(Ty::string())], Ty::bool())
                }
                TypingAttr::BinOp(TypingBinOp::Add) => {
                    Ty::function(vec![Param::pos_only(Ty::string())], Ty::string())
                }
                TypingAttr::BinOp(TypingBinOp::Mul) => {
                    Ty::function(vec![Param::pos_only(Ty::int())], Ty::string())
                }
                TypingAttr::Slice => Ty::string(),
                TypingAttr::BinOp(TypingBinOp::Percent) => {
                    Ty::function(vec![Param::pos_only(Ty::Any)], Ty::string())
                }
                TypingAttr::Regular(name) => match x.attr(name) {
                    Ok(res) => res,
                    Err(()) => return Some(Err(())),
                },
                _ => return fallback(),
            },
            Ty::Name(x) if x == "range" => match attr {
                TypingAttr::Iter => Ty::int(),
                TypingAttr::BinOp(TypingBinOp::In) => {
                    Ty::function(vec![Param::pos_only(Ty::int())], Ty::bool())
                }
                _ => return Some(Err(())),
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
