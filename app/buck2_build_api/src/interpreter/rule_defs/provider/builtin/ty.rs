/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;
use std::sync::OnceLock;

use buck2_interpreter::types::provider::callable::ProviderCallableLike;
use dupe::Dupe;
use starlark::environment::GlobalsBuilder;
use starlark::typing::Ty;
use starlark::typing::TyStarlarkValue;
use starlark::values::function::FUNCTION_TYPE;
use starlark::values::typing::TypeInstanceId;
use starlark::values::StarlarkValue;
use starlark_map::sorted_map::SortedMap;

use crate::interpreter::rule_defs::provider::ty::provider::ty_provider;
use crate::interpreter::rule_defs::provider::ty::provider_callable::ty_provider_callable;
use crate::interpreter::rule_defs::provider::ProviderLike;

/// Types associated with builtin providers.
pub struct BuiltinProviderTy<
    'v,
    P: StarlarkValue<'v> + ProviderLike<'v>,
    C: StarlarkValue<'v> + ProviderCallableLike,
> {
    callable: OnceLock<Ty>,
    instance: OnceLock<Ty>,
    phantom: PhantomData<&'v (P, C)>,
}

unsafe impl<
    'v,
    P: StarlarkValue<'v> + ProviderLike<'v>,
    C: StarlarkValue<'v> + ProviderCallableLike,
> Sync for BuiltinProviderTy<'v, P, C>
{
}

impl<'v, P: StarlarkValue<'v> + ProviderLike<'v>, C: StarlarkValue<'v> + ProviderCallableLike>
    BuiltinProviderTy<'v, P, C>
{
    pub const fn new() -> BuiltinProviderTy<'v, P, C> {
        BuiltinProviderTy {
            callable: OnceLock::new(),
            instance: OnceLock::new(),
            phantom: PhantomData,
        }
    }

    pub fn callable(&self, creator_func: for<'a> fn(&'a mut GlobalsBuilder)) -> Ty {
        self.callable
            .get_or_init(|| builtin_provider_typechecker_ty::<C>(creator_func))
            .dupe()
    }

    pub fn instance(&self) -> Ty {
        self.instance
            .get_or_init(|| {
                ty_provider(
                    P::TYPE,
                    TypeInstanceId::gen(),
                    TyStarlarkValue::new::<P>(),
                    None,
                    SortedMap::new(),
                )
                .unwrap()
            })
            .dupe()
    }
}

fn builtin_provider_typechecker_ty<'v, C: StarlarkValue<'v> + ProviderCallableLike>(
    creator_func: for<'a> fn(&'a mut GlobalsBuilder),
) -> Ty {
    let globals = GlobalsBuilder::new().with(creator_func).build();
    let mut iter = globals.iter();
    let Some(first) = iter.next() else {
        panic!("empty globals");
    };
    if iter.next().is_some() {
        panic!("more then one global in creator func globals");
    }
    if first.1.to_value().get_type() != FUNCTION_TYPE {
        panic!("creator func is not a function");
    }
    let ty = Ty::of_value(first.1.to_value());
    let ty_callable = ty
        .as_function()
        .expect("creator func is not a function")
        .callable()
        .dupe();
    ty_provider_callable::<C>(ty_callable).unwrap()
}
