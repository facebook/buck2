/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[macro_export]
macro_rules! buck2_env {
    ($var:literal) => {{
        static ENV_HELPER: $crate::env::helper::EnvHelper<std::string::String> =
            $crate::env::helper::EnvHelper::new_from_macro($var);
        let v: anyhow::Result<Option<&'static str>> = ENV_HELPER.get()
            .map(|option| option.map(|v| v.as_str()));
        v
    }};
    ($var:literal, type=$ty:ty) => {{
        static ENV_HELPER: $crate::env::helper::EnvHelper<$ty> =
            $crate::env::helper::EnvHelper::new_from_macro($var);
        let v: anyhow::Result<Option<$ty>> = ENV_HELPER.get().map(|option| option.copied());
        v
    }};
    ($var:literal, type=$ty:ty, default=$default:expr) => {{
        let v: anyhow::Result<$ty> = buck2_env!($var, type=$ty)
            .map(|option| option.unwrap_or_else(|| $default));
        v
    }};
    ($var:literal, bool) => {{
        let v: anyhow::Result<bool> = buck2_env!($var, type=bool, default=false);
        v
    }};
    ($var:literal, type=$ty:ty, converter=$converter:expr) => {{
        static ENV_HELPER: $crate::env::helper::EnvHelper<$ty> =
            $crate::env::helper::EnvHelper::with_converter_from_macro($var, $converter);
        let v: anyhow::Result<Option<&$ty>> = ENV_HELPER.get();
        v
    }};
}

pub use buck2_env;
