/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::{environment::GlobalsBuilder, values::Value};

use crate::interpreter::rule_defs::provider::{callable::ProviderCallableLike, ProviderLike};

pub(crate) struct ProviderRegistration {
    pub(crate) as_provider_callable: fn(Value) -> Option<&dyn ProviderCallableLike>,
    pub(crate) as_provider: fn(Value) -> Option<&dyn ProviderLike>,
    pub(crate) register_globals: fn(&mut GlobalsBuilder),
}

inventory::collect!(ProviderRegistration);

pub fn register_builtin_providers(registry: &mut GlobalsBuilder) {
    for registration in inventory::iter::<ProviderRegistration> {
        (registration.register_globals)(registry);
    }
}
