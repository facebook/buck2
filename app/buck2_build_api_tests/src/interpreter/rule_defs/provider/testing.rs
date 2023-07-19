/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::provider::callable::register_provider;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_build_api::interpreter::rule_defs::provider::registration::register_builtin_providers;
use buck2_interpreter_for_build::attrs::coerce;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Module;

pub trait FrozenProviderCollectionValueExt {
    /// Creates a `FrozenProviderCollectionValue` for testing. The given string should be
    /// Starlark code that returns a list of providers. The built in providers are available.
    fn testing_new(providers: &str) -> Self;
}

impl FrozenProviderCollectionValueExt for FrozenProviderCollectionValue {
    fn testing_new(providers: &str) -> Self {
        let env = Module::new();
        let globals = GlobalsBuilder::extended()
            .with(register_builtin_providers)
            .with(register_provider)
            .build();
        let value = coerce::testing::to_value(&env, &globals, providers);
        let res_typed =
            buck2_build_api::interpreter::rule_defs::provider::collection::ProviderCollection::try_from_value(value)
                .map_err(|e| anyhow::anyhow!("{:?}", e))
                .unwrap();

        let res = env.heap().alloc(res_typed);
        env.set("", res);

        let frozen_env = env.freeze().expect("should freeze successfully");
        let res = frozen_env.get("").unwrap();

        FrozenProviderCollectionValue::try_from_value(res)
            .expect("just created this, this shouldn't happen")
    }
}
