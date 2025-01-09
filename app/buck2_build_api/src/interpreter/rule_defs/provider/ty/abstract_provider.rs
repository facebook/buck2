/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::OnceLock;

use allocative::Allocative;
use buck2_interpreter::late_binding_ty::ProviderReprLate;
use dupe::Dupe;
use starlark::typing::Ty;
use starlark::typing::TyStarlarkValue;
use starlark::typing::TyUser;
use starlark::typing::TyUserFields;
use starlark::typing::TyUserParams;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::typing::TypeInstanceId;
use starlark::values::typing::TypeMatcher;
use starlark::values::typing::TypeMatcherFactory;
use starlark::values::StarlarkValue;
use starlark::values::Value;

use crate::interpreter::rule_defs::provider::user::UserProvider;
use crate::interpreter::rule_defs::provider::ValueAsProviderLike;

#[derive(Allocative, Clone, Debug)]
struct ProviderMatcher;

impl TypeMatcher for ProviderMatcher {
    fn matches(&self, value: Value) -> bool {
        ValueAsProviderLike::unpack(value).is_some()
    }
}

fn mk_ty_provider() -> buck2_error::Result<Ty> {
    Ok(Ty::custom(TyUser::new(
        UserProvider::TYPE.to_owned(),
        // Builtin providers behave like `UserProvider`.
        TyStarlarkValue::new::<UserProvider>(),
        TypeInstanceId::gen(),
        TyUserParams {
            matcher: Some(TypeMatcherFactory::new(ProviderMatcher)),
            fields: TyUserFields::unknown(),
            ..TyUserParams::default()
        },
    )?))
}

/// Type of any provider instance. In Starlark it is available as `Provider`.
pub struct AbstractProvider;

impl StarlarkTypeRepr for AbstractProvider {
    type Canonical = Self;

    fn starlark_type_repr() -> Ty {
        static TY: OnceLock<Ty> = OnceLock::new();
        TY.get_or_init(|| mk_ty_provider().unwrap()).dupe()
    }
}

pub(crate) fn init_provider_ty() {
    ProviderReprLate::init(AbstractProvider::starlark_type_repr())
}
