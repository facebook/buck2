/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![doc(hidden)]

pub use linkme;

/// This macro is used to register environment variables that are used by Buck2.
///
/// The first argument to the macro must always be a string literal with the name of the environment
/// variable.
///
/// Additionally, you can specify the following, comma separated:
///
///  - `type=<type>` - the Rust type that the environment variable should be converted to, using
///    `FromStr::from_str`. Defaults to `&'static str` if not specified.
///  - `default=<value>` - an expression for the default value to use if the environment variable is
///    not set.
///  - `converter=<expr>` - a function to use as an alternative to the `FromStr::from_str`
///    conversion. Must have signature `fn(&str) -> Result<Ty, E>`
///
/// The macro expands to an expression of type `anyhow::Result<Type>` if a default is set, and
/// `anyhow::Result<Option<Type>` otherwise.
pub macro buck2_env {
    ($var:literal) => {{
        $crate::env::macros::register!($var, ty=std::string::String, default=std::option::Option::None);
        static ENV_HELPER: $crate::env::helper::EnvHelper<std::string::String> =
            $crate::env::helper::EnvHelper::new_from_macro($var);
        let v: anyhow::Result<Option<&'static str>> = ENV_HELPER.get()
            .map(|option| option.map(|v| v.as_str()));
        v
    }},
    ($var:literal, type=$ty:ty) => {{
        $crate::env::macros::register!($var, ty=$ty, default=None);
        static ENV_HELPER: $crate::env::helper::EnvHelper<$ty> =
            $crate::env::helper::EnvHelper::new_from_macro($var);
        let v: anyhow::Result<Option<$ty>> = ENV_HELPER.get_copied();
        v
    }},
    ($var:literal, type=$ty:ty, default=$default:expr) => {{
        $crate::env::macros::register!($var, ty=$ty, default=std::option::Option::Some(stringify!($default)));
        static ENV_HELPER: $crate::env::helper::EnvHelper<$ty> =
            $crate::env::helper::EnvHelper::new_from_macro($var);
        let v: anyhow::Result<$ty> = ENV_HELPER.get_copied()
            .map(|option| option.unwrap_or_else(|| $default));
        v
    }},
    ($var:literal, bool) => {{
        let v: anyhow::Result<bool> = buck2_env!($var, type=bool, default=false);
        v
    }},
    ($var:literal, type=$ty:ty, converter=$converter:expr) => {{
        $crate::env::macros::register!($var, ty=$ty, default=std::option::Option::None);
        static ENV_HELPER: $crate::env::helper::EnvHelper<$ty> =
            $crate::env::helper::EnvHelper::with_converter_from_macro($var, $converter);
        let v: anyhow::Result<Option<&$ty>> = ENV_HELPER.get();
        v
    }},
}

pub macro register($var:literal, ty=$ty:ty, default=$default:expr) {{
    use $crate::env::macros::linkme;
    #[linkme::distributed_slice($crate::env::registry::ENV_INFO)]
    #[linkme(crate = $crate::env::macros::linkme)]
    static ENV_INFO: $crate::env::registry::EnvInfoEntry = $crate::env::registry::EnvInfoEntry {
        name: $var,
        ty: stringify!($ty),
        default: $default,
    };
}}
