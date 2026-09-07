/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use sorted_vector_map::SortedVectorMap;

use crate::data::ArgValue;
use crate::data::ArgValueContent;
use crate::data::ExternalRunnerSpecValue;

pub type TestEnvironment = SortedVectorMap<String, ArgValue>;

/// Build a test environment and add a UTF-8 `LC_CTYPE` when it is not already defined.
/// We select `C.UTF-8` on Unices because it has deterministic behaviour and widely available.
/// macOS and Windows get `en_US.UTF-8` because `C.UTF-8` is not available.
/// In practice, this means that iconv related functions in libc will not corrupt or error out on
/// UTF-8 text.
///
/// The caller supplies the test runner's process value so precedence can be tested without
/// mutating the process environment. Test-runner CLI overrides, if any, should be applied after
/// this function returns.
pub fn build_test_env(
    spec_env: impl IntoIterator<Item = (String, ExternalRunnerSpecValue)>,
    process_lc_ctype: Option<String>,
) -> TestEnvironment {
    let mut env = spec_env
        .into_iter()
        .map(|(key, value)| {
            (
                key,
                ArgValue {
                    content: ArgValueContent::ExternalRunnerSpecValue(value),
                    format: None,
                },
            )
        })
        .collect::<TestEnvironment>();

    if env.contains_key("LC_CTYPE") {
        return env;
    }

    let lc_ctype = process_lc_ctype.or_else(|| default_lc_ctype().map(str::to_owned));
    if let Some(lc_ctype) = lc_ctype {
        env.insert(
            "LC_CTYPE".to_owned(),
            ArgValue {
                content: ArgValueContent::ExternalRunnerSpecValue(
                    ExternalRunnerSpecValue::Verbatim(lc_ctype),
                ),
                format: None,
            },
        );
    }

    env
}

#[cfg(target_os = "macos")]
fn default_lc_ctype() -> Option<&'static str> {
    Some("en_US.UTF-8")
}

#[cfg(all(unix, not(target_os = "macos")))]
fn default_lc_ctype() -> Option<&'static str> {
    Some("C.UTF-8")
}

#[cfg(windows)]
fn default_lc_ctype() -> Option<&'static str> {
    // This form is accepted by the UCRT and by Unix-like compatibility runtimes on Windows.
    Some("en_US.UTF-8")
}

#[cfg(not(any(unix, windows)))]
fn default_lc_ctype() -> Option<&'static str> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lc_ctype(env: &SortedVectorMap<String, ArgValue>) -> Option<&str> {
        match env.get("LC_CTYPE").map(|value| &value.content) {
            Some(ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::Verbatim(
                value,
            ))) => Some(value),
            _ => None,
        }
    }

    #[test]
    fn test_existing_value_wins_over_process() {
        let env = build_test_env(
            vec![(
                "LC_CTYPE".to_owned(),
                ExternalRunnerSpecValue::Verbatim("from-test".to_owned()),
            )],
            Some("from-process".to_owned()),
        );
        assert_eq!(lc_ctype(&env), Some("from-test"));
    }

    #[test]
    fn test_process_value_wins_over_default() {
        let env = build_test_env(vec![], Some("from-process".to_owned()));
        assert_eq!(lc_ctype(&env), Some("from-process"));
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn test_platform_default() {
        let env = build_test_env(vec![], None);
        let expected = if cfg!(all(unix, not(target_os = "macos"))) {
            "C.UTF-8"
        } else {
            "en_US.UTF-8"
        };
        assert_eq!(lc_ctype(&env), Some(expected));
    }
}
