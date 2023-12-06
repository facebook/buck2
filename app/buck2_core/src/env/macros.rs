/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod __macro_refs {
    pub use linkme;
}

#[macro_export]
macro_rules! buck2_env {
    (register $var:literal, ty=$ty:ty, default=$default: expr) => {
        {
            use $crate::env::macros::__macro_refs::linkme;
            #[linkme::distributed_slice($crate::env::registry::ENV_INFO)]
            #[linkme(crate = $crate::env::macros::__macro_refs::linkme)]
            static ENV_INFO: $crate::env::registry::EnvInfoEntry = $crate::env::registry::EnvInfoEntry {
                name: $var,
                ty: stringify!($ty),
                default: $default,
            };
        }
    };
    ($var:literal) => {{
        buck2_env!(register $var, ty=std::string::String, default=std::option::Option::None);
        static ENV_HELPER: $crate::env::helper::EnvHelper<std::string::String> =
            $crate::env::helper::EnvHelper::new_from_macro($var);
        let v: anyhow::Result<Option<&'static str>> = ENV_HELPER.get()
            .map(|option| option.map(|v| v.as_str()));
        v
    }};
    ($var:literal, type=$ty:ty) => {{
        buck2_env!(register $var, ty=$ty, default=None);
        static ENV_HELPER: $crate::env::helper::EnvHelper<$ty> =
            $crate::env::helper::EnvHelper::new_from_macro($var);
        let v: anyhow::Result<Option<$ty>> = ENV_HELPER.get().map(|option| option.copied());
        v
    }};
    ($var:literal, type=$ty:ty, default=$default:expr) => {{
        buck2_env!(register $var, ty=$ty, default=std::option::Option::Some(stringify!($default)));
        static ENV_HELPER: $crate::env::helper::EnvHelper<$ty> =
            $crate::env::helper::EnvHelper::new_from_macro($var);
        let v: anyhow::Result<$ty> = ENV_HELPER.get().map(|option| option.copied())
            .map(|option| option.unwrap_or_else(|| $default));
        v
    }};
    ($var:literal, bool) => {{
        let v: anyhow::Result<bool> = buck2_env!($var, type=bool, default=false);
        v
    }};
    ($var:literal, type=$ty:ty, converter=$converter:expr) => {{
        buck2_env!(register $var, ty=$ty, default=std::option::Option::None);
        static ENV_HELPER: $crate::env::helper::EnvHelper<$ty> =
            $crate::env::helper::EnvHelper::with_converter_from_macro($var, $converter);
        let v: anyhow::Result<Option<&$ty>> = ENV_HELPER.get();
        v
    }};
}

pub use buck2_env;
