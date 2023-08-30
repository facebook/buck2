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

use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use starlark_syntax::codemap::Span;
use starlark_syntax::codemap::Spanned;

use crate::typing::custom::TyCustomImpl;
use crate::typing::error::TypingOrInternalError;
use crate::typing::Arg;
use crate::typing::Ty;
use crate::typing::TyBasic;
use crate::typing::TypingOracleCtx;
use crate::values::enumeration::ty_enum_value::TyEnumValue;
use crate::values::enumeration::EnumType;
use crate::values::types::type_instance_id::TypeInstanceId;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::typing::type_compiled::factory::TypeCompiledFactory;
use crate::values::StarlarkValue;
use crate::values::Value;

#[derive(Allocative, Ord, PartialOrd, Debug)]
pub(crate) struct TyEnumData {
    /// Name of the enum type.
    pub(crate) name: String,
    /// Types of variants.
    pub(crate) variants: Vec<Ty>,
    /// Globally unique id of the enum type.
    // Id must be last so `Ord` is deterministic.
    pub(crate) id: TypeInstanceId,
}

impl PartialEq for TyEnumData {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TyEnumData {}

impl std::hash::Hash for TyEnumData {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Do not hash `id` because hashing should be deterministic.
        self.name.hash(state);
        self.variants.hash(state);
    }
}

/// Type of enum type, i.e. type of `enum()`.
#[derive(
    Debug,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Hash,
    Clone,
    Dupe,
    Allocative,
    derive_more::Display
)]
#[display(fmt = "enum[name = \"{}\"]", "self.data.name")]
pub struct TyEnumType {
    /// This is `Arc` so `TyEnum` could grab `TyEnumType`.
    pub(crate) data: Arc<TyEnumData>,
}

impl TyCustomImpl for TyEnumType {
    fn as_name(&self) -> Option<&str> {
        Some(EnumType::TYPE)
    }

    fn iter_item(&self) -> Result<Ty, ()> {
        Ok(Ty::custom(TyEnumValue {
            enum_type: self.dupe(),
        }))
    }

    fn index(&self, _item: &TyBasic) -> Result<Ty, ()> {
        // TODO(nga): more precise function type.
        Ok(Ty::any_function())
    }

    fn attribute(&self, attr: &str) -> Result<Ty, ()> {
        match attr {
            "type" => Ok(Ty::string()),
            "values" => {
                // TODO(nga): more precise function type.
                Ok(Ty::any_function())
            }
            _ => Err(()),
        }
    }

    fn is_callable(&self) -> bool {
        true
    }

    fn validate_call(
        &self,
        _span: Span,
        _args: &[Spanned<Arg>],
        _oracle: TypingOracleCtx,
    ) -> Result<Ty, TypingOrInternalError> {
        // TODO(nga): validate args.
        Ok(Ty::custom(TyEnumValue {
            enum_type: self.dupe(),
        }))
    }

    fn matcher<'v>(&self, factory: TypeCompiledFactory<'v>) -> TypeCompiled<Value<'v>> {
        let _ignore = factory;
        // TODO(nga): replace panic with error.
        unreachable!("Cannot appear in type expressions")
    }
}
