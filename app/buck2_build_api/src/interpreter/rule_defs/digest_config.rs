/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_execute::digest_config::DigestConfig;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

/// Functions to access the daemon's digest config. This is not normally all that useful, but it
/// allows instrospection in a few useful cases (such as knowing what hashes the daemon will be
/// able to defer).
#[derive(
    Debug,
    Clone,
    Dupe,
    Freeze,
    Display,
    Trace,
    ProvidesStaticType,
    NoSerialize,
    Allocative
)]
#[display("{}", self.digest_config)]
pub struct StarlarkDigestConfig {
    #[freeze(identity)]
    pub digest_config: DigestConfig,
}

#[starlark_value(type = "DigestConfig", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkDigestConfig {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(digest_config_methods)
    }
}

impl<'v> AllocValue<'v> for StarlarkDigestConfig {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        heap.alloc_simple(self)
    }
}

#[starlark_module]
fn digest_config_methods(builder: &mut MethodsBuilder) {
    fn allows_sha1(this: &StarlarkDigestConfig) -> starlark::Result<bool> {
        Ok(this.digest_config.cas_digest_config().allows_sha1())
    }

    fn allows_sha256(this: &StarlarkDigestConfig) -> starlark::Result<bool> {
        Ok(this.digest_config.cas_digest_config().allows_sha256())
    }

    fn allows_blake3(this: &StarlarkDigestConfig) -> starlark::Result<bool> {
        Ok(this.digest_config.cas_digest_config().allows_blake3())
    }

    fn allows_blake3_keyed(this: &StarlarkDigestConfig) -> starlark::Result<bool> {
        Ok(this.digest_config.cas_digest_config().allows_blake3_keyed())
    }
}

#[starlark_module]
pub(crate) fn register_digest_config_type(globals: &mut GlobalsBuilder) {
    const DigestConfig: StarlarkValueAsType<StarlarkDigestConfig> = StarlarkValueAsType::new();
}
