/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Mutex;

use once_cell::sync::OnceCell;

use crate::env_helper::EnvHelper;

type SoftErrorHandler =
    Box<dyn Fn(&'static str, &anyhow::Error, (&'static str, u32, u32)) + Send + Sync + 'static>;

static HANDLER: OnceCell<SoftErrorHandler> = OnceCell::new();

static ALL_SOFT_ERROR_COUNTERS: Mutex<Vec<&'static AtomicUsize>> = Mutex::new(Vec::new());

/// Throw a "soft_error" i.e. one that is destined to become a hard error
/// in the near future. The macro lives in this crate to allow it be
/// made available everywhere. Calling programs are responsible for
/// calling initialize() to provide a handler for logging these soft_errors.
///
/// You should pass two arguments:
///
/// * The category string that will remain constant and identifies this specific soft error
///   (used to report as a key).
/// * The error is an `anyhow::Error` will in the future will be propagated as the error.
///
/// Soft errors from Meta internal runs can be viewed
/// [in logview](https://www.internalfb.com/logview/overview/buck2).
#[macro_export]
macro_rules! soft_error(
    ($category:expr, $err:expr) => { {
        static COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        static ONCE: std::sync::Once = std::sync::Once::new();
        $crate::error::handle_soft_error($category, $err, &COUNT, &ONCE, (file!(), line!(), column!()), false)
    } }
);

/// Like [`soft_error!`] but don't print to the console. Used to turn on the soft error quietly for
/// a few days to tackle the most significant issues before informing users.
#[macro_export]
macro_rules! quiet_soft_error(
    ($category:expr, $err:expr) => { {
        static COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        static ONCE: std::sync::Once = std::sync::Once::new();
        $crate::error::handle_soft_error($category, $err, &COUNT, &ONCE, (file!(), line!(), column!()), true)
    } }
);

// Hidden because an implementation detail of `soft_error!`.
#[doc(hidden)]
pub fn handle_soft_error(
    category: &'static str,
    err: anyhow::Error,
    count: &'static AtomicUsize,
    once: &std::sync::Once,
    loc: (&'static str, u32, u32),
    quiet: bool,
) -> anyhow::Result<()> {
    once.call_once(|| {
        ALL_SOFT_ERROR_COUNTERS.lock().unwrap().push(count);
    });

    static HARD_ERROR: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_HARD_ERROR");

    // We want to limit each error to appearing at most 10 times in a build (no point spamming people)
    if count.fetch_add(1, Ordering::SeqCst) < 10 {
        if !quiet {
            tracing::warn!("Important warning at {}:{}:{} {}", loc.0, loc.1, loc.2, err);
        }
        if let Some(handler) = HANDLER.get() {
            handler(category, &err, loc);
        }
    }

    if HARD_ERROR.get_copied().ok() == Some(Some(true)) {
        eprintln!("Important warning caused failure due to $BUCK2_TEST_HARD_ERROR");
        Err(err)
    } else {
        Ok(())
    }
}

#[allow(clippy::significant_drop_in_scrutinee)] // False positive.
pub fn reset_soft_error_counters() {
    for counter in ALL_SOFT_ERROR_COUNTERS.lock().unwrap().iter() {
        counter.store(0, Ordering::Relaxed);
    }
}

pub fn initialize(handler: SoftErrorHandler) {
    if let Err(_e) = HANDLER.set(handler) {
        panic!("Cannot initialize soft_error handler more than once");
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Mutex;
    use std::sync::MutexGuard;
    use std::sync::Once;

    use crate::error::initialize;
    use crate::error::reset_soft_error_counters;
    use crate::soft_error;

    static RESULT: Mutex<Vec<String>> = Mutex::new(Vec::new());

    fn mock_handler(category: &'static str, err: &anyhow::Error, loc: (&'static str, u32, u32)) {
        RESULT
            .lock()
            .unwrap()
            .push(format!("{:?}, : {} : {}", loc, err, category));
    }

    fn test_init() -> MutexGuard<'static, ()> {
        // Tests in Rust can be executed concurrently, and these tests work with global state,
        // so use mutex to ensure we only run one test at a time.
        static TEST_MUTEX: Mutex<()> = Mutex::new(());
        let guard = TEST_MUTEX.lock().unwrap();

        static ONCE: Once = Once::new();
        ONCE.call_once(|| {
            initialize(box mock_handler);
        });

        RESULT.lock().unwrap().clear();

        guard
    }

    #[test]
    fn test_soft_error() {
        let _guard = test_init();

        let before_error_line = line!();
        let _ignore_hard_error = soft_error!(
            "test_logged_soft_error",
            anyhow::anyhow!("Should be logged")
        );
        assert_eq!(
            Some(&format!(
                "({:?}, {}, 34), : Should be logged : test_logged_soft_error",
                file!(),
                before_error_line + 1,
            )),
            RESULT.lock().unwrap().get(0)
        );
    }

    #[test]
    fn test_reset_counters() {
        let _guard = test_init();

        assert_eq!(0, RESULT.lock().unwrap().len(), "Sanity check");

        for _ in 0..100 {
            let _ignore = soft_error!("test_reset_counters", anyhow::anyhow!("Message"));
        }

        assert_eq!(
            10,
            RESULT.lock().unwrap().len(),
            "Should be logged 10 times"
        );

        reset_soft_error_counters();

        for _ in 0..100 {
            let _ignore = soft_error!("test_reset_counters", anyhow::anyhow!("Message"));
        }

        assert_eq!(
            20,
            RESULT.lock().unwrap().len(),
            "Should be logged 10 more times"
        );
    }
}
