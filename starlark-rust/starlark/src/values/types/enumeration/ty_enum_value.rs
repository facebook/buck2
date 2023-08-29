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

use allocative::Allocative;
use dupe::Dupe;

use crate::typing::custom::TyCustomImpl;
use crate::typing::oracle::traits::TypingAttr;
use crate::typing::Ty;
use crate::values::enumeration::ty_enum_type::TyEnumType;
use crate::values::enumeration::EnumValue;
use crate::values::types::type_instance_id::TypeInstanceId;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::typing::type_compiled::compiled::TypeCompiledImpl;
use crate::values::typing::type_compiled::factory::TypeCompiledFactory;
use crate::values::Value;

/// Type of enum variant, i.e. type of `enum()[0]`.
#[derive(
    Debug,
    Allocative,
    derive_more::Display,
    Clone,
    Dupe,
    Eq,
    PartialEq,
    Hash,
    Ord,
    PartialOrd
)]
#[display(fmt = "enum(name = \"{}\", ...)", "enum_type.data.name")]
pub(crate) struct TyEnumValue {
    pub(crate) enum_type: TyEnumType,
}

impl TyCustomImpl for TyEnumValue {
    fn as_name(&self) -> Option<&str> {
        Some(&self.enum_type.data.name)
    }

    fn attribute(&self, _attr: TypingAttr) -> Result<Ty, ()> {
        // TODO(nga): better types.
        Ok(Ty::any())
    }

    fn matcher<'v>(&self, factory: TypeCompiledFactory<'v>) -> TypeCompiled<Value<'v>> {
        #[derive(Hash, Debug, Eq, PartialEq, Clone, Dupe, Allocative)]
        struct EnumTypeMatcher {
            id: TypeInstanceId,
        }

        impl TypeCompiledImpl for EnumTypeMatcher {
            fn matches(&self, value: Value) -> bool {
                match EnumValue::from_value(value) {
                    None => false,
                    Some(en) => en.id == self.id,
                }
            }
        }

        factory.alloc(EnumTypeMatcher {
            id: self.enum_type.data.id,
        })
    }

    fn intersects(x: &Self, y: &Self) -> bool {
        x == y
    }
}
