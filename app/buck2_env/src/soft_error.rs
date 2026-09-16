/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::RefCell;
use std::str::FromStr;
use std::sync::Arc;
use std::sync::LazyLock;
use std::sync::OnceLock;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use buck2_hash::BuckDashMap;
use starlark_map::small_set::SmallSet;

use crate::env::buck2_env;

type StructuredErrorHandler = Box<
    dyn for<'a> Fn(
            &'a str,
            &buck2_error::Error,
            (&'a str, u32, u32),
            &Arc<SoftErrorContext>,
            StructuredErrorOptions,
        ) + Send
        + Sync
        + 'static,
>;

static HANDLER: OnceLock<StructuredErrorHandler> = OnceLock::new();

/// Supplies the soft-error context associated with the current command, when one exists.
pub type SoftErrorContextProvider =
    Box<dyn Fn() -> Option<Arc<SoftErrorContext>> + Send + Sync + 'static>;

static CONTEXT_PROVIDER: OnceLock<SoftErrorContextProvider> = OnceLock::new();
// Work with no command dispatcher shares one process-lifetime policy and emission quota.
static FALLBACK_CONTEXT: OnceLock<Arc<SoftErrorContext>> = OnceLock::new();

thread_local! {
    static THREAD_CONTEXT: RefCell<Option<Arc<SoftErrorContext>>> = const { RefCell::new(None) };
}

struct RestoreSoftErrorContext(Option<Arc<SoftErrorContext>>);

impl Drop for RestoreSoftErrorContext {
    fn drop(&mut self) {
        THREAD_CONTEXT.with(|context| {
            context.replace(self.0.take());
        });
    }
}

pub fn buck2_hard_error_env() -> buck2_error::Result<Option<&'static str>> {
    buck2_env!("BUCK2_HARD_ERROR")
}

#[derive(Debug, PartialEq, Eq)]
enum ShowSoftErrorConfig {
    Disabled,
    All,
    Selected(SmallSet<String>),
}

impl ShowSoftErrorConfig {
    fn should_show(&self, category: &str) -> bool {
        match self {
            Self::Disabled => false,
            Self::All => true,
            Self::Selected(s) => s.contains(category),
        }
    }
}

impl ShowSoftErrorConfig {
    fn parse(s: &str) -> Self {
        if s.is_empty() {
            return Self::Disabled;
        }
        let val = s.to_lowercase();
        if val == "true" || val == "1" {
            return Self::All;
        }
        if let Some(categories) = val.strip_prefix("only=") {
            return Self::Selected(categories.split(',').map(|c| c.trim().to_owned()).collect());
        }
        Self::Disabled
    }
}

pub fn buck2_show_soft_errors_env() -> buck2_error::Result<Option<&'static str>> {
    buck2_env!("BUCK2_SHOW_SOFT_ERRORS")
}

static HARD_ERROR_PANIC_ALLOWLIST: LazyLock<SmallSet<String>> =
    LazyLock::new(|| SmallSet::from_iter(["spawn_version_control_collector_failed".to_owned()]));

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
///
/// Example (see [StructuredErrorOptions] for all key=value options):
/// ```ignore
/// soft_error!(
///     "soft_error_category",
///     buck2_error::buck2_error!(
///         buck2_error::ErrorTag::Tier0,
///         "Did something bad with {}",
///         value,
///     )
///     .into(),
///     quiet = false,
/// )?;
/// ```
pub macro soft_error {
    ($category:expr, $err:expr) => {
        $crate::soft_error::soft_error!($category, $err,)
    },
    ($category:expr, $err:expr, $($k:ident : $v:expr),+) => {
        $crate::soft_error::soft_error!($category, $err, $($k: $v,)*)
    },
    ($category:expr, $err:expr, $($k:ident : $v:expr ,)*) => { {
        $crate::soft_error::handle_soft_error(
            $category,
            $err,
            (file!(), line!(), column!()),
            $crate::soft_error::StructuredErrorOptions {
                $($k: $v,)*
                ..Default::default()
            }
        )
    } },
}

/// Tag and report this error. Return said error.
pub macro tag_error {
    ($category:expr, $err:expr) => {
        $crate::soft_error::tag_error!($category, $err,)
    },
    ($category:expr, $err:expr, $($k:ident : $v:expr),+) => {
        $crate::soft_error::tag_error!($category, $err, $($k: $v,)*)
    },
    ($category:expr, $err:expr, $($k:ident : $v:expr ,)*) => {
        match $crate::soft_error::soft_error!($category, $err, $($k: $v,)*) {
            Ok(err) => err,
            Err(err) => err,
        }
    },
}

/// If this result is an error, tag it, then return the result.
pub macro tag_result {
    ($category:expr, $res:expr) => {
        $crate::soft_error::tag_result($category, $res,)
    },
    ($category:expr, $err:expr, $($k:ident : $v:expr),+) => {
        $crate::soft_error::tag_result!($category, $err, $($k: $v,)*)
    },
    ($category:expr, $res:expr, $($k:ident : $v:expr ,)*) => {
        $res.map_err(|err| $crate::soft_error::tag_error!($category, err, $($k: $v,)*))
    },
}

pub struct StructuredErrorOptions {
    /// Log this error (to our event log and possibly to a task), but do not print it to stderr.
    pub quiet: bool,
    /// Create a task for this error.
    pub task: bool,
    pub deprecation: bool,
    /// When true, this soft error will be promoted to a hard error in open source builds.
    /// Use this for deprecation/migration errors that OSS users should see.
    /// Monitoring/logging errors should leave this as false (the default).
    pub error_on_oss: bool,
    pub daemon_in_memory_state_is_corrupted: bool,
    pub daemon_materializer_state_is_corrupted: bool,
    pub action_cache_is_corrupted: bool,
    // By default, we only get a handful of traces per error category in Logview.
    // This key, if specified, enables logging one trace per unique key using
    // the "trace cut" feature of Logview. Note that the dimensionality of this
    // key must not be too large otherwise it can bring significant capacity cost
    // and may even bring down Logview.
    pub low_cardinality_key_for_additional_logview_samples: Option<Box<dyn ToString>>,
}

impl Default for StructuredErrorOptions {
    fn default() -> Self {
        Self {
            quiet: true,
            task: true,
            deprecation: false,
            error_on_oss: false,
            daemon_in_memory_state_is_corrupted: false,
            daemon_materializer_state_is_corrupted: false,
            action_cache_is_corrupted: false,
            low_cardinality_key_for_additional_logview_samples: None,
        }
    }
}

// Hidden because an implementation detail of `soft_error!`.
#[doc(hidden)]
pub fn handle_soft_error(
    category: &str,
    err: buck2_error::Error,
    loc: (&'static str, u32, u32),
    options: StructuredErrorOptions,
) -> Result<buck2_error::Error, buck2_error::Error> {
    validate_logview_category(category)?;

    let context = soft_error_context()?;

    let mut options = options;
    if options.quiet && context.show_soft_error_config.should_show(category) {
        options.quiet = false;
    }

    let error_on_oss = options.error_on_oss;

    // We want to limit each error to appearing at most 10 times in a build (no point spamming people)
    if context.should_emit(loc) {
        if let Some(handler) = HANDLER.get() {
            handler(category, &err, loc, &context, options);
        }
    }

    if context.hard_error_config.should_panic(category) {
        panic!(
            "Upgraded warning to panic via $BUCK2_HARD_ERROR\n {category}: {:?}",
            err
        );
    }
    if context.hard_error_config.should_hard_error(category) {
        return Err(err.context("Upgraded warning to failure via $BUCK2_HARD_ERROR"));
    }

    // @oss-disable: let is_open_source = false;
    let is_open_source = true; // @oss-enable
    if is_open_source && error_on_oss {
        // In open source builds, only deprecation/migration soft errors (those with
        // error_on_oss: true) are promoted to hard errors. Monitoring/logging soft errors
        // are no-ops, matching internal behavior.
        return Err(err);
    }

    Ok(err)
}

fn soft_error_context() -> buck2_error::Result<Arc<SoftErrorContext>> {
    if let Some(context) = capture_soft_error_context() {
        return Ok(context);
    }

    if let Some(context) = FALLBACK_CONTEXT.get() {
        return Ok(context.clone());
    }

    let context = Arc::new(SoftErrorContext::from_environment()?);
    let _ignored = FALLBACK_CONTEXT.set(context.clone());
    Ok(FALLBACK_CONTEXT.get().cloned().unwrap_or(context))
}

/// Captures command-scoped policy without manufacturing a process fallback.
pub fn capture_soft_error_context() -> Option<Arc<SoftErrorContext>> {
    THREAD_CONTEXT
        .with(|context| context.borrow().clone())
        .or_else(|| CONTEXT_PROVIDER.get().and_then(|provider| provider()))
}

/// Installs captured command policy while executing synchronous work on another thread.
pub fn with_soft_error_context<R>(
    context: Option<Arc<SoftErrorContext>>,
    func: impl FnOnce() -> R,
) -> R {
    let Some(context) = context else {
        return func();
    };

    THREAD_CONTEXT.with(|current| {
        let previous = current.replace(Some(context));
        let _restore = RestoreSoftErrorContext(previous);
        func()
    })
}

pub fn initialize(
    handler: StructuredErrorHandler,
    context_provider: SoftErrorContextProvider,
) -> buck2_error::Result<()> {
    soft_error_context()?;

    if let Err(_e) = CONTEXT_PROVIDER.set(context_provider) {
        panic!("Cannot initialize SoftErrorContextProvider more than once");
    }

    if let Err(_e) = HANDLER.set(handler) {
        panic!("Cannot initialize StructuredErrorHandler handler more than once");
    }

    Ok(())
}

/// Policy and rate-limit state shared by work attributed to one command.
#[derive(Debug)]
pub struct SoftErrorContext {
    hard_error_config: HardErrorConfig,
    show_soft_error_config: ShowSoftErrorConfig,
    counts: BuckDashMap<(&'static str, u32, u32), AtomicUsize>,
    command_scoped: bool,
}

impl SoftErrorContext {
    /// Parses the client-provided soft-error policy for a command.
    pub fn new(hard_error_config: &str, show_soft_error_config: &str) -> buck2_error::Result<Self> {
        Ok(Self {
            hard_error_config: HardErrorConfig::from_str(hard_error_config)?,
            show_soft_error_config: ShowSoftErrorConfig::parse(show_soft_error_config),
            counts: BuckDashMap::default(),
            command_scoped: true,
        })
    }

    fn from_environment() -> buck2_error::Result<Self> {
        let mut context = Self::new(
            buck2_hard_error_env()?.unwrap_or_default(),
            buck2_show_soft_errors_env()?.unwrap_or_default(),
        )?;
        context.command_scoped = false;
        Ok(context)
    }

    /// Whether this context belongs to a client command rather than process-wide fallback work.
    pub fn is_command_scoped(&self) -> bool {
        self.command_scoped
    }

    fn should_emit(&self, loc: (&'static str, u32, u32)) -> bool {
        if let Some(count) = self.counts.get(&loc) {
            return increment_soft_error_counter(&count);
        }

        let count = self.counts.entry(loc).or_default();
        increment_soft_error_counter(&count)
    }
}

fn increment_soft_error_counter(count: &AtomicUsize) -> bool {
    count
        .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |count| {
            (count < 10).then_some(count + 1)
        })
        .is_ok()
}

/// Parse either a boolean or `only=category1,category2`
#[derive(Debug, PartialEq, Eq)]
enum HardErrorConfig {
    Bool(bool),
    Selected(SmallSet<String>),
    Panic,
}

impl HardErrorConfig {
    fn should_panic(&self, category: &str) -> bool {
        match self {
            Self::Panic => !HARD_ERROR_PANIC_ALLOWLIST.contains(category),
            _ => false,
        }
    }

    fn should_hard_error(&self, category: &str) -> bool {
        match self {
            Self::Bool(v) => *v,
            Self::Selected(s) => s.contains(category),
            Self::Panic => true, // category is in HARD_ERROR_PANIC_ALLOWLIST, make it a normal hard error
        }
    }
}

impl FromStr for HardErrorConfig {
    type Err = InvalidHardErrorConfig;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Ok(Self::Bool(false));
        }

        if s == "panic" {
            return Ok(Self::Panic);
        }

        if let Ok(v) = s.parse() {
            return Ok(Self::Bool(v));
        }

        let mut parts = s.split('=');

        if let (Some("only"), Some(v), None) = (parts.next(), parts.next(), parts.next()) {
            return Ok(Self::Selected(
                v.split(',').map(|s| s.trim().to_owned()).collect(),
            ));
        }

        Err(InvalidHardErrorConfig(s.to_owned()))
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
    fn test_soft_error_contexts_are_isolated() -> buck2_error::Result<()> {
        let first = SoftErrorContext::new("true", "")?;
        let second = SoftErrorContext::new("false", "only=test_category")?;
        let loc = ("test.rs", 1, 1);

        assert!(first.hard_error_config.should_hard_error("test_category"));
        assert!(!first.show_soft_error_config.should_show("test_category"));
        assert!(!second.hard_error_config.should_hard_error("test_category"));
        assert!(second.show_soft_error_config.should_show("test_category"));

        for _ in 0..10 {
            assert!(first.should_emit(loc));
        }
        assert!(!first.should_emit(loc));
        assert!(second.should_emit(loc));

        Ok(())
    }

    #[test]
    fn test_soft_error_context_limits_concurrent_emission() -> buck2_error::Result<()> {
        let context = Arc::new(SoftErrorContext::new("", "")?);
        let emitted = std::thread::scope(|scope| {
            (0..4)
                .map(|_| {
                    let context = context.clone();
                    scope.spawn(move || {
                        (0..100)
                            .filter(|_| context.should_emit(("test.rs", 1, 1)))
                            .count()
                    })
                })
                .map(|handle| handle.join().expect("worker should not panic"))
                .sum::<usize>()
        });

        assert_eq!(10, emitted);
        Ok(())
    }

    #[test]
    fn test_soft_error_context_sync_scope_restores_nested_context() -> buck2_error::Result<()> {
        let outer = Arc::new(SoftErrorContext::new("", "")?);
        let inner = Arc::new(SoftErrorContext::new("", "")?);

        with_soft_error_context(Some(outer.clone()), || {
            with_soft_error_context(Some(inner.clone()), || {
                let observed = capture_soft_error_context()
                    .expect("inner context should be visible on the worker thread");
                assert!(Arc::ptr_eq(&inner, &observed));
            });
            let observed = capture_soft_error_context()
                .expect("outer context should be restored on the worker thread");
            assert!(Arc::ptr_eq(&outer, &observed));
        });

        assert!(capture_soft_error_context().is_none());
        Ok(())
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
