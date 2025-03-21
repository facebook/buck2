/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;

use arc_swap::ArcSwapOption;
use buck2_error::BuckErrorContext;
use starlark_map::small_set::SmallSet;

use crate::env::__macro_refs::buck2_env;
use crate::is_open_source;

type StructuredErrorHandler = Box<
    dyn for<'a> Fn(&'a str, &buck2_error::Error, (&'a str, u32, u32), StructuredErrorOptions)
        + Send
        + Sync
        + 'static,
>;

static HANDLER: OnceLock<StructuredErrorHandler> = OnceLock::new();

pub fn buck2_hard_error_env() -> buck2_error::Result<Option<&'static str>> {
    buck2_env!("BUCK2_HARD_ERROR")
}

static HARD_ERROR_CONFIG: HardErrorConfigHolder = HardErrorConfigHolder {
    config: ArcSwapOption::const_empty(),
};

static ALL_SOFT_ERROR_COUNTERS: Mutex<Vec<&'static AtomicUsize>> = Mutex::new(Vec::new());

/// Throw a "soft_error" ie. a non-fatal error logged to logview.
/// Errors will not be logged to stderr as warnings to the user, unless `quiet=false` is passed.
/// Logview will generate tasks for each error category, unless `task=false` is passed.
/// If `deprecation=true` this error should ideally become a hard error in the future.
///
/// The macro lives in this crate to allow it be made available everywhere.
/// Calling programs are responsible for calling initialize() to provide a handler for
/// logging these soft_errors.
///
/// You should pass two arguments:
///
/// * The category string that will remain constant and identifies this specific soft error
///   (used to report as a key).
/// * The error is a `buck2_error::Error`.
///
/// Soft errors from Meta internal runs can be viewed
/// [in logview](https://www.internalfb.com/logview/overview/buck2).
///
/// You'll get the error back as the Ok() value if it wasn't thrown, otherwise you get a Err() to
/// propagate.
#[macro_export]
macro_rules! soft_error(
    ($category:expr, $err:expr) => {
        $crate::soft_error!($category, $err,)
    };
    ($category:expr, $err:expr, $($k:ident : $v:expr),+) => {
        $crate::soft_error!($category, $err, $($k: $v,)*)
    };
    ($category:expr, $err:expr, $($k:ident : $v:expr ,)*) => { {
        static COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        static ONCE: std::sync::Once = std::sync::Once::new();
        $crate::error::handle_soft_error(
            $category,
            $err,
            &COUNT,
            &ONCE,
            (file!(), line!(), column!()),
            $crate::error::StructuredErrorOptions {
                $($k: $v,)*
                ..Default::default()
            }
        )
    } };
);

/// Tag and report this error. Return said error.
#[macro_export]
macro_rules! tag_error(
    ($category:expr, $err:expr) => {
        $crate::tag_error!($category, $err,)
    };
    ($category:expr, $err:expr, $($k:ident : $v:expr),+) => {
        $crate::tag_error!($category, $err, $($k: $v,)*)
    };
    ($category:expr, $err:expr, $($k:ident : $v:expr ,)*) => {
        match $crate::soft_error!($category, $err, $($k: $v,)*) {
            Ok(err) => err,
            Err(err) => err,
        }
    };
);

/// If this result is an error, tag it, then return the result.
#[macro_export]
macro_rules! tag_result(
    ($category:expr, $res:expr) => {
        $crate::tag_result($category, $res,)
    };
    ($category:expr, $err:expr, $($k:ident : $v:expr),+) => {
        $crate::tag_result!($category, $err, $($k: $v,)*)
    };
    ($category:expr, $res:expr, $($k:ident : $v:expr ,)*) => {
        $res.map_err(|err| $crate::tag_error!($category, err, $($k: $v,)*))
    };
);

fn hard_error_config() -> buck2_error::Result<Arc<HardErrorConfig>> {
    // This function should return `Guard<Arc<HardErrorConfig>>` to make it a little bit faster,
    // see https://github.com/vorner/arc-swap/issues/90

    if let Some(config) = HARD_ERROR_CONFIG.config.load_full() {
        return Ok(config);
    }

    let config = buck2_hard_error_env()?.unwrap_or_default();
    let config = HardErrorConfig::from_str(config)?;
    HARD_ERROR_CONFIG.config.store(Some(Arc::new(config)));
    HARD_ERROR_CONFIG
        .config
        .load_full()
        .internal_error("Just stored a value")
}

pub fn reload_hard_error_config(var_value: &str) -> buck2_error::Result<()> {
    HARD_ERROR_CONFIG.reload_hard_error_config(var_value)
}

pub struct StructuredErrorOptions {
    /// Log this error (to our event log and possibly to a task), but do not print it to stderr.
    pub quiet: bool,
    /// Create a task for this error.
    pub task: bool,
    pub deprecation: bool,
    pub daemon_in_memory_state_is_corrupted: bool,
    pub daemon_materializer_state_is_corrupted: bool,
    pub action_cache_is_corrupted: bool,
}

impl Default for StructuredErrorOptions {
    fn default() -> Self {
        Self {
            quiet: true,
            task: true,
            deprecation: false,
            daemon_in_memory_state_is_corrupted: false,
            daemon_materializer_state_is_corrupted: false,
            action_cache_is_corrupted: false,
        }
    }
}

// Hidden because an implementation detail of `soft_error!`.
#[doc(hidden)]
pub fn handle_soft_error(
    category: &str,
    err: buck2_error::Error,
    count: &'static AtomicUsize,
    once: &std::sync::Once,
    loc: (&'static str, u32, u32),
    options: StructuredErrorOptions,
) -> Result<buck2_error::Error, buck2_error::Error> {
    validate_logview_category(category)?;

    if cfg!(test) {
        // When running unit tests of `buck2_core` crate, all errors are hard errors.
        return Err(err);
    }

    once.call_once(|| {
        ALL_SOFT_ERROR_COUNTERS.lock().unwrap().push(count);
    });

    // We want to limit each error to appearing at most 10 times in a build (no point spamming people)
    if count.fetch_add(1, Ordering::SeqCst) < 10 {
        if let Some(handler) = HANDLER.get() {
            handler(category, &err, loc, options);
        }
    }

    if hard_error_config()?.should_hard_error(category) {
        return Err(err
            .context("Upgraded warning to failure via $BUCK2_HARD_ERROR")
            .into());
    }

    if is_open_source() {
        // We don't log these, and we have no legacy users, and they might not upgrade that often,
        // so lets just break open source things immediately.
        return Err(err);
    }

    Ok(err)
}

#[allow(clippy::significant_drop_in_scrutinee)] // False positive.
pub fn reset_soft_error_counters() {
    for counter in ALL_SOFT_ERROR_COUNTERS.lock().unwrap().iter() {
        counter.store(0, Ordering::Relaxed);
    }
}

pub fn initialize(handler: StructuredErrorHandler) -> buck2_error::Result<()> {
    hard_error_config()?;

    if let Err(_e) = HANDLER.set(handler) {
        panic!("Cannot initialize StructuredErrorHandler handler more than once");
    }

    Ok(())
}

/// Parse either a boolean or `only=category1,category2`
#[derive(Debug, PartialEq, Eq)]
enum HardErrorConfig {
    Bool(bool),
    Selected(SmallSet<String>),
}

impl HardErrorConfig {
    fn should_hard_error(&self, category: &str) -> bool {
        match self {
            Self::Bool(v) => *v,
            Self::Selected(s) => s.contains(category),
        }
    }
}

impl FromStr for HardErrorConfig {
    type Err = InvalidHardErrorConfig;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Ok(Self::Bool(false));
        }

        if let Ok(v) = s.parse() {
            return Ok(Self::Bool(v));
        }

        let mut parts = s.split('=');

        match (parts.next(), parts.next(), parts.next()) {
            (Some("only"), Some(v), None) => {
                return Ok(Self::Selected(
                    v.split(',').map(|s| s.trim().to_owned()).collect(),
                ));
            }
            _ => {}
        }

        Err(InvalidHardErrorConfig(s.to_owned()))
    }
}

struct HardErrorConfigHolder {
    config: ArcSwapOption<HardErrorConfig>,
}

impl HardErrorConfigHolder {
    fn reload_hard_error_config(&self, var_value: &str) -> buck2_error::Result<()> {
        let config = HardErrorConfig::from_str(var_value)?;
        if let Some(old_config) = &*self.config.load() {
            if **old_config == config {
                return Ok(());
            }
        }

        self.config.store(Some(Arc::new(config)));
        Ok(())
    }
}

#[derive(buck2_error::Error, Debug)]
#[error(
    "Invalid hard error config: `{0}`\n\
    Valid examples: empty, `true`, `false`, `only=category1,category2`"
)]
#[buck2(tag = Input)]
struct InvalidHardErrorConfig(String);

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
enum InvalidSoftError {
    #[error("Invalid category, must be lower_snake_case, got `{0}`")]
    InvalidCategory(String),
}

/// A category must be a-z with no consecutive underscores. Or we raise an error.
pub fn validate_logview_category(category: &str) -> buck2_error::Result<()> {
    let mut allow_underscore = false;
    for &x in category.as_bytes() {
        if x.is_ascii_lowercase() {
            allow_underscore = true;
        } else if allow_underscore && x == b'_' {
            allow_underscore = false;
        } else {
            // Go to the shared error path
            allow_underscore = false;
            break;
        }
    }
    if !allow_underscore {
        Err(InvalidSoftError::InvalidCategory(category.to_owned()).into())
    } else {
        Ok(())
    }
}

#[cfg(test)]
pub(crate) mod tests {

    use assert_matches::assert_matches;

    use super::*;

    #[test]
    fn test_hard_error() -> buck2_error::Result<()> {
        assert!(HardErrorConfig::from_str("true")?.should_hard_error("foo"));
        assert!(!HardErrorConfig::from_str("false")?.should_hard_error("foo"));
        assert_eq!(
            HardErrorConfig::Bool(false),
            HardErrorConfig::from_str("")?,
            "Empty string must parse to no hard errors"
        );
        assert!(!HardErrorConfig::from_str("")?.should_hard_error("foo"));

        assert!(HardErrorConfig::from_str("only=foo,bar")?.should_hard_error("foo"));
        assert!(!HardErrorConfig::from_str("only=foo,bar")?.should_hard_error("baz"));

        Ok(())
    }

    #[test]
    fn test_reset_soft_error_handler() {
        let config = HardErrorConfigHolder {
            config: ArcSwapOption::const_empty(),
        };

        assert!(config.config.load().is_none());

        config.reload_hard_error_config("true").unwrap();
        let c0 = config.config.load();
        let c0 = c0.as_ref().unwrap();
        config.reload_hard_error_config("true").unwrap();
        let c1 = config.config.load();
        let c1 = c1.as_ref().unwrap();
        assert!(Arc::ptr_eq(c0, c1), "Reload identical config is no-op");
        assert_eq!(**c0, HardErrorConfig::Bool(true));

        config.reload_hard_error_config("false").unwrap();
        let c2 = config.config.load();
        let c2 = c2.as_ref().unwrap();
        assert_eq!(**c2, HardErrorConfig::Bool(false));
    }

    #[test]
    fn test_validate_logview_category() {
        assert_matches!(validate_logview_category("valid"), Ok(_));
        assert_matches!(validate_logview_category("a_valid_category"), Ok(_));
        assert_matches!(validate_logview_category(""), Err(_));
        assert_matches!(validate_logview_category("Invalid_because_capital"), Err(_));
        assert_matches!(validate_logview_category("some_1number"), Err(_));
        assert_matches!(validate_logview_category("two__underscore"), Err(_));
        assert_matches!(validate_logview_category("a-dash"), Err(_));
        assert_matches!(validate_logview_category("_leading_underscore"), Err(_));
        assert_matches!(validate_logview_category("trailing_underscore_"), Err(_));
    }
}
