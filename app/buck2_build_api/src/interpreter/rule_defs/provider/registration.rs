/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use starlark::environment::GlobalsBuilder;

pub struct ProviderRegistration {
    pub register_globals: fn(&mut GlobalsBuilder),
}

inventory::collect!(ProviderRegistration);

pub fn register_builtin_providers(registry: &mut GlobalsBuilder) {
    for registration in inventory::iter::<ProviderRegistration> {
        (registration.register_globals)(registry);
    }
}
