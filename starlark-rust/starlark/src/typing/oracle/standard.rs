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

use std::collections::HashMap;

use crate::docs::Doc;
use crate::docs::Identifier;
use crate::environment::Globals;
use crate::stdlib::LibraryExtension;
use crate::typing::oracle::docs::OracleDocs;
use crate::typing::oracle::traits::TypingOracle;
use crate::typing::ty::Arg;
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
        self.fallback.builtin(name)
    }

    fn builtin_call(&self, _name: &str, _args: &[Arg]) -> Option<Result<Ty, String>> {
        None
    }

    fn subtype(&self, _require: &TyName, _got: &TyName) -> bool {
        false
    }
}
