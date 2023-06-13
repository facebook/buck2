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

use std::collections::BTreeMap;

use crate::docs::Doc;
use crate::docs::DocItem;
use crate::environment::Globals;
use crate::stdlib::LibraryExtension;
use crate::typing::oracle::docs::OracleDocs;
use crate::typing::oracle::traits::TypingOracle;
use crate::typing::ty::Arg;
use crate::typing::ty::Param;
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
        add::<crate::values::record::FrozenField>(&mut fallback);
        add::<crate::values::record::FrozenRecordType>(&mut fallback);
        add::<crate::values::regex::StarlarkRegex>(&mut fallback);
        add::<crate::values::dict::value::DictGen<crate::values::dict::value::FrozenDictData>>(
            &mut fallback,
        );
        add::<crate::values::list::value::ListGen<crate::values::list::value::FrozenListData>>(
            &mut fallback,
        );
        add::<crate::values::string::StarlarkStr>(&mut fallback);
        add::<crate::values::structs::value::FrozenStruct>(&mut fallback);
        add::<crate::values::tuple::value::FrozenTuple>(&mut fallback);

        Self { fallback }
    }
}

impl TypingOracle for OracleStandard {
    fn attribute(&self, ty: &Ty, attr: &str) -> Option<Result<Ty, ()>> {
        let fallback = || match self.fallback.attribute(ty, attr) {
            // We know we have full knowledge, so if we don't know, that must mean the attribute does not exist
            None => Some(Err(())),
            x => x,
        };
        // We have to explicitly implement operators (e.g. `__in__` since we don't generate documentation for them).
        // We explicitly implement polymorphic functions (e.g. `dict.get`) so they can get much more precise types.
        Some(Ok(match ty {
            Ty::None => return Some(Err(())),
            Ty::List(elem) => match attr {
                "__slice__" => ty.clone(),
                "__less__" => {
                    // This is a bit weak, beacuse it only looks at this oracle
                    return self.attribute(ty, attr);
                }
                "__iter__" => (**elem).clone(),
                "__in__" => Ty::function(vec![Param::pos_only((**elem).clone())], Ty::bool()),
                "__index__" => Ty::function(vec![Param::pos_only(Ty::int())], (**elem).clone()),
                "__add__" => Ty::function(vec![Param::pos_only(Ty::Any)], Ty::list(Ty::Any)),
                "__multiply__" => Ty::function(vec![Param::pos_only(Ty::int())], ty.clone()),
                "pop" => Ty::function(
                    vec![Param::pos_only(Ty::int()).optional()],
                    (**elem).clone(),
                ),
                "index" => Ty::function(
                    vec![
                        Param::pos_only((**elem).clone()),
                        Param::pos_only(Ty::int()).optional(),
                    ],
                    Ty::int(),
                ),
                "remove" => Ty::function(vec![Param::pos_only((**elem).clone())], Ty::None),
                _ => return fallback(),
            },
            Ty::Dict(tk_tv) => {
                let (ref tk, ref tv) = **tk_tv;
                match attr {
                    "__in__" => Ty::function(vec![Param::pos_only(tk.clone())], Ty::bool()),
                    "__bitor__" => Ty::function(vec![Param::pos_only(ty.clone())], ty.clone()),
                    "__iter__" => tk.clone(),
                    "__index__" => Ty::function(vec![Param::pos_only(tk.clone())], tv.clone()),
                    "get" => Ty::union2(
                        Ty::function(
                            vec![Param::pos_only(tk.clone())],
                            Ty::union2(tv.clone(), Ty::None),
                        ),
                        // This second signature is a bit too lax, but get with a default is much rarer
                        Ty::function(
                            vec![Param::pos_only(tk.clone()), Param::pos_only(Ty::Any)],
                            Ty::Any,
                        ),
                    ),
                    "keys" => Ty::function(vec![], Ty::list(tk.clone())),
                    "values" => Ty::function(vec![], Ty::list(tv.clone())),
                    "items" => {
                        Ty::function(vec![], Ty::list(Ty::Tuple(vec![tk.clone(), tv.clone()])))
                    }
                    "popitem" => Ty::function(vec![], Ty::Tuple(vec![tk.clone(), tv.clone()])),
                    _ => return fallback(),
                }
            }
            Ty::Struct { fields, extra } => match fields.get(attr) {
                Some(ty) => ty.clone(),
                None if *extra => Ty::Any,
                _ => return Some(Err(())),
            },
            Ty::Tuple(tys) => match attr {
                "__in__" => {
                    Ty::function(vec![Param::pos_only(Ty::unions(tys.clone()))], Ty::bool())
                }
                "__iter__" => Ty::unions(tys.clone()),
                "__index__" => {
                    Ty::function(vec![Param::pos_only(Ty::int())], Ty::unions(tys.clone()))
                }
                _ => return Some(Err(())),
            },
            Ty::Name(x) if x == "tuple" => match attr {
                "__iter__" => Ty::Any,
                "__in__" => Ty::function(vec![Param::pos_only(Ty::Any)], Ty::bool()),
                "__index__" => Ty::function(vec![Param::pos_only(Ty::int())], Ty::Any),
                _ => return Some(Err(())),
            },
            Ty::Name(x) if x == "int" => match attr {
                "__less__" => Ty::function(vec![Param::pos_only(Ty::int())], Ty::bool()),
                "__subtract__" => Ty::function(vec![Param::pos_only(Ty::int())], Ty::int()),
                "__add__" => Ty::function(vec![Param::pos_only(Ty::int())], Ty::int()),
                "__minus__" => Ty::function(vec![], Ty::int()),
                "__divide__" => Ty::function(vec![Param::pos_only(Ty::Any)], Ty::float()),
                _ => return Some(Err(())),
            },
            Ty::Name(x) if x == "float" => match attr {
                "__less__" => Ty::function(vec![Param::pos_only(Ty::float())], Ty::bool()),
                "__subtract__" => Ty::function(vec![Param::pos_only(Ty::float())], Ty::float()),
                "__add__" => Ty::function(vec![Param::pos_only(Ty::float())], Ty::float()),
                "__minus__" => Ty::function(vec![], Ty::float()),
                "__divide__" => Ty::function(vec![Param::pos_only(Ty::Any)], Ty::float()),
                _ => return Some(Err(())),
            },
            Ty::Name(x) if x == "string" => match attr {
                "__less__" => Ty::function(vec![Param::pos_only(Ty::string())], Ty::bool()),
                "__index__" => Ty::function(vec![Param::pos_only(Ty::int())], Ty::string()),
                "__in__" => Ty::function(vec![Param::pos_only(Ty::string())], Ty::bool()),
                "__add__" => Ty::function(vec![Param::pos_only(Ty::string())], Ty::string()),
                "__multiply__" => Ty::function(vec![Param::pos_only(Ty::int())], Ty::string()),
                "__slice__" => Ty::string(),
                "__percent__" => Ty::function(vec![Param::pos_only(Ty::Any)], Ty::string()),
                _ => return fallback(),
            },
            Ty::Name(x) if x == "range" => match attr {
                "__iter__" => Ty::int(),
                "__in__" => Ty::function(vec![Param::pos_only(Ty::int())], Ty::bool()),
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

    fn builtin(&self, name: &str) -> Option<Result<Ty, ()>> {
        Some(Ok(match name {
            "None" => Ty::None,
            "True" | "False" => Ty::bool(),
            "zip" => Ty::special_function("zip", vec![Param::args(Ty::Any)], Ty::list(Ty::Any)),
            "struct" => Ty::special_function(
                "struct",
                vec![Param::kwargs(Ty::Any)],
                Ty::Struct {
                    fields: BTreeMap::new(),
                    extra: true,
                },
            ),
            _ => return self.fallback.builtin(name),
        }))
    }

    fn builtin_call(&self, name: &str, args: &[Arg]) -> Option<Result<Ty, String>> {
        match name {
            "struct" => {
                let mut fields = BTreeMap::new();
                let mut extra = false;
                for x in args {
                    match x {
                        Arg::Pos(_) | Arg::Args(_) => {
                            return Some(Err("Positional arguments not allowed".to_owned()));
                        }
                        Arg::Name(name, val) => {
                            fields.insert(name.clone(), val.clone());
                        }
                        Arg::Kwargs(_) => extra = true,
                    }
                }
                Some(Ok(Ty::Struct { fields, extra }))
            }
            "zip" => {
                let mut res = Vec::new();
                for x in args {
                    match x {
                        Arg::Pos(x) => match self.attribute(x, "__iter__") {
                            Some(Err(_)) => {
                                return Some(Err("Argument does not allow iteration".to_owned()));
                            }
                            Some(Ok(t)) => res.push(t),
                            // We are only asking our own oracle, not all the oracles about the iter attribute.
                            // So will get to this approximation even if an externally defined iterable type exists.
                            None => res.push(Ty::Any),
                        },
                        Arg::Args(_) => return Some(Ok(Ty::name("tuple"))),
                        _ => return Some(Err("Named arguments not allowed".to_owned())),
                    }
                }
                Some(Ok(Ty::list(Ty::Tuple(res))))
            }
            _ => None,
        }
    }

    fn subtype(&self, _require: &TyName, _got: &TyName) -> bool {
        false
    }
}
