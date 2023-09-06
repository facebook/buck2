/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::types::provider::callable::ProviderCallableLike;
use starlark::typing::Ty;
use starlark::typing::TyFunction;
use starlark::typing::TyStarlarkValue;
use starlark::typing::TyUser;
use starlark::typing::TyUserFields;
use starlark::values::typing::TypeInstanceId;
use starlark::values::StarlarkValue;

pub(crate) fn ty_provider_callable<'v, C: StarlarkValue<'v> + ProviderCallableLike>(
    creator_func: TyFunction,
) -> anyhow::Result<Ty> {
    Ok(Ty::custom(TyUser::new(
        C::TYPE.to_owned(),
        TyStarlarkValue::new::<C>(),
        None,
        TypeInstanceId::gen(),
        TyUserFields::no_fields(),
        Some(creator_func),
        None,
        None,
    )?))
}
