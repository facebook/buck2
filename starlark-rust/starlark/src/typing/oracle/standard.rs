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
use std::collections::HashMap;

use crate::docs::Doc;
use crate::docs::Identifier;
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
        let mut fallback =
            OracleDocs::new_object(&Globals::extended_by(extensions).documentation());

        fn add<T: StarlarkValue<'static>>(fallback: &mut OracleDocs) {
            if let Some(m) = T::get_methods() {
                fallback.add_doc(&Doc {
                    id: Identifier {
                        name: T::TYPE.to_owned(),
                        location: None,
                    },
                    item: m.documentation(),
                    custom_attrs: HashMap::new(),
                });
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
        // TODO: Don't fall back for __ attributes that we own
        self.fallback.attribute(ty, attr)
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
