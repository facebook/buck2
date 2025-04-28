/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![doc(hidden)]

use std::str::FromStr;

pub use linkme;

pub fn convert_from_str<T>(v: &str) -> buck2_error::Result<T>
where
    T: FromStr,
    buck2_error::Error: From<<T as FromStr>::Err>,
{
    Ok(T::from_str(v)?)
}

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
///  - `applicability=<internal|testing>` - to indicate that the variable is not used in OSS or only
///    for self-testing of buck2
///
/// The macro expands to an expression of type `buck2_error::Result<Type>` if a default is set, and
/// `buck2_error::Result<Option<Type>` otherwise.
pub macro buck2_env {
    ($var:expr, bool $(, $($rest:tt)*)?) => {{
        let v: buck2_error::Result<bool> = $crate::env::__macro_refs::buck2_env!($var, type=bool, default=false, converter = |s| {
            match s.to_lowercase().as_str() {
                "1" | "true" => Ok(true),
                "0" | "false" => Ok(false),
                _ => Err(buck2_error::buck2_error!(buck2_error::ErrorTag::Tier0, "Invalid bool value: {}", s).into()),
            }
        }, $($($rest)*)?);
        v
    }},
    ($var:expr, type=$ty:ty, default=$default:expr, converter=$converter:expr $(, $($rest:tt)*)?) => {{
        $crate::env::__macro_refs::parse2!(
            (
            var=$var,
            parser=$converter,
            stored_type=$ty,
            processor=|x| x.copied().unwrap_or_else(|| $default),
            output_type=$ty,
            default_repr=std::option::Option::Some(stringify!($default)),
            ),
            $($($rest)*)?
        )
    }},
    ($var:expr, type=$ty:ty, default=$default:expr $(, $($rest:tt)*)?) => {{
        $crate::env::__macro_refs::parse2!(
            (
            var=$var,
            parser=$crate::env::__macro_refs::convert_from_str,
            stored_type=$ty,
            processor=|x| x.copied().unwrap_or_else(|| $default),
            output_type=$ty,
            default_repr=std::option::Option::Some(stringify!($default)),
            ),
            $($($rest)*)?
        )
    }},
    ($var:expr, type=$ty:ty, converter=$converter:expr $(, $($rest:tt)*)?) => {{
        $crate::env::__macro_refs::parse2!(
            (
            var=$var,
            parser=$converter,
            stored_type=$ty,
            processor=|x| x,
            output_type=std::option::Option<&$ty>,
            default_repr=std::option::Option::None,
            ),
            $($($rest)*)?
        )
    }},
    ($var:expr, type=$ty:ty $(, $($rest:tt)*)?) => {{
        $crate::env::__macro_refs::parse2!(
            (
            var=$var,
            parser=$crate::env::__macro_refs::convert_from_str,
            stored_type=$ty,
            processor=|x| x.copied(),
            output_type=std::option::Option<$ty>,
            default_repr=std::option::Option::None,
            ),
            $($($rest)*)?
        )
    }},
    ($var:expr $(, $($rest:tt)*)?) => {{
        $crate::env::__macro_refs::parse2!(
            (
            var=$var,
            parser=$crate::env::__macro_refs::convert_from_str,
            stored_type=std::string::String,
            processor=|x| x.map(|x| x.as_str()),
            output_type=std::option::Option<&'static str>,
            default_repr=std::option::Option::None,
            ),
            $($($rest)*)?
        )
    }},
}

/// Register env name to be shown in `buck2 help-env`.
pub macro buck2_env_name($var:expr) {{
    $crate::env::__macro_refs::register!(
        $var,
        ty = std::string::String,
        default = std::option::Option::None,
        applicability = $crate::env::registry::Applicability::All
    );

    $var
}}

#[allow(unused_macros)]
macro parse2 {
    (
        $already_parsed:tt,
        applicability=internal$(,)?
    ) => {
        $crate::env::__macro_refs::expand!($already_parsed, applicability=$crate::env::registry::Applicability::Internal,)
    },
    (
        $already_parsed:tt,
        applicability=testing$(,)?
    ) => {
        $crate::env::__macro_refs::expand!($already_parsed, applicability=$crate::env::registry::Applicability::Testing,)
    },
    (
        $already_parsed:tt,
        $(,)?
    ) => {
        $crate::env::__macro_refs::expand!($already_parsed, applicability=$crate::env::registry::Applicability::All,)
    },
}

#[allow(unused_macros)]
/// `parser` is `&str -> buck2_error::Result<$stored_type>`, `processor` is `Option<& $stored_type> -> $output_type`
///
/// The extra set of parentheses is a trick to let us pass things through `parse2` transparently
macro expand(
    (
    var=$var:expr,
    parser=$parser:expr,
    stored_type=$stored_ty:ty,
    processor=$processor:expr,
    output_type=$output_ty:ty,
    default_repr=$default_repr:expr,
    ),
    applicability=$applicability:expr,
) {{
    $crate::env::__macro_refs::register!(
        $var,
        ty = $stored_ty,
        default = $default_repr,
        applicability = $applicability
    );
    static ENV_HELPER: $crate::env::helper::EnvHelper<$stored_ty> =
        $crate::env::helper::EnvHelper::with_converter_from_macro($var, $parser);
    let v: buck2_error::Result<$output_ty> = ENV_HELPER.get().map($processor);
    v
}}

macro register($var:expr, ty=$ty:ty, default=$default:expr, applicability=$applicability:expr) {{
    use $crate::env::__macro_refs::linkme;
    #[linkme::distributed_slice($crate::env::registry::ENV_INFO)]
    #[linkme(crate = $crate::env::__macro_refs::linkme)]
    static ENV_INFO: $crate::env::registry::EnvInfoEntry = $crate::env::registry::EnvInfoEntry {
        name: $var,
        ty: stringify!($ty),
        default: $default,
        applicability: $applicability,
    };
}}
