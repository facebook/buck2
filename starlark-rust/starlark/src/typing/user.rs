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

use std::cmp::Ordering;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use dupe::OptionDupedExt;
use starlark_map::sorted_map::SortedMap;
use starlark_syntax::codemap::Span;
use starlark_syntax::codemap::Spanned;

use crate::typing::custom::TyCustomImpl;
use crate::typing::error::TypingOrInternalError;
use crate::typing::starlark_value::TyStarlarkValue;
use crate::typing::Arg;
use crate::typing::Ty;
use crate::typing::TypingOracleCtx;
use crate::values::types::type_instance_id::TypeInstanceId;
use crate::values::typing::type_compiled::alloc::TypeMatcherAlloc;
use crate::values::typing::type_compiled::type_matcher_factory::TypeMatcherFactory;

/// Type description for arbitrary type.
#[derive(Allocative, Debug, derive_more::Display)]
#[display(fmt = "{}", name)]
pub(crate) struct TyUser {
    pub(crate) name: String,
    /// Base type for this custom type, e.g. generic record for record with known fields.
    pub(crate) base: TyStarlarkValue,
    pub(crate) matcher: Option<TypeMatcherFactory>,
    pub(crate) id: TypeInstanceId,
    pub(crate) fields: SortedMap<String, Ty>,
}

impl PartialEq for TyUser {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TyUser {}

impl PartialOrd for TyUser {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TyUser {
    fn cmp(&self, other: &Self) -> Ordering {
        (&self.name, &self.fields, self.id).cmp(&(&other.name, &self.fields, other.id))
    }
}

impl Hash for TyUser {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.fields.hash(state);
    }
}

impl TyCustomImpl for TyUser {
    fn as_name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn attribute(&self, attr: &str) -> Result<Ty, ()> {
        if let Ok(ty) = self.base.attr_from_methods(attr) {
            Ok(ty)
        } else {
            self.fields.get(attr).duped().ok_or(())
        }
    }

    fn is_callable(&self) -> bool {
        self.base.is_callable()
    }

    fn validate_call(
        &self,
        span: Span,
        _args: &[Spanned<Arg>],
        oracle: TypingOracleCtx,
    ) -> Result<Ty, TypingOrInternalError> {
        Ok(self.base.validate_call(span, oracle)?)
    }

    fn matcher<T: TypeMatcherAlloc>(&self, factory: T) -> T::Result {
        match &self.matcher {
            Some(matcher) => factory.from_type_matcher_factory(matcher),
            None => factory.unreachable_cannot_appear_in_type_expr(),
        }
    }

    fn intersects(x: &Self, y: &Self) -> bool {
        x == y
    }
}
