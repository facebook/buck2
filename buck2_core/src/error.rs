use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use once_cell::sync::OnceCell;

use crate::env_helper::EnvHelper;

type SoftErrorHandler =
    Box<dyn Fn(&'static str, &anyhow::Error, (&'static str, u32, u32)) + Send + Sync + 'static>;

static HANDLER: OnceCell<SoftErrorHandler> = OnceCell::new();

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
#[macro_export]
macro_rules! soft_error(
    ($category:expr, $err:expr) => { {
        static COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        buck2_core::error::handle_soft_error($category, $err, &COUNT, (file!(), line!(), column!()))
    } }
);

pub fn handle_soft_error(
    category: &'static str,
    err: anyhow::Error,
    count: &AtomicUsize,
    loc: (&'static str, u32, u32),
) -> anyhow::Result<()> {
    static HARD_ERROR: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_HARD_ERROR");

    // We want to limit each error to appearing at most 10 times in a build (no point spamming people)
    if count.fetch_add(1, Ordering::SeqCst) <= 10 {
        tracing::warn!("Important warning at {}:{}:{} {}", loc.0, loc.1, loc.2, err);
        if let Some(handler) = HANDLER.get() {
            handler(category, &err, loc);
        }
    }

    if HARD_ERROR.get().ok() == Some(&Some(true)) {
        // We can't anyhow::Error out of here because we don't return a Result.
        // We don't want to panic out of here in case that goes into our panic handler.
        // So cheat and drop an `exit` (this branch is only ever taken in our tests).
        eprintln!("Important warning caused failure due to $BUCK2_TEST_HARD_ERROR");
        Err(err)
    } else {
        Ok(())
    }
}

pub fn initialize(handler: SoftErrorHandler) {
    if let Err(_e) = HANDLER.set(handler) {
        panic!("Cannot initialize soft_error handler more than once");
    }
}

#[cfg(test)]
mod test {
    use std::sync::Mutex;

    use once_cell::sync::OnceCell;

    use crate as buck2_core;
    use crate::soft_error;

    static RESULT: OnceCell<Mutex<Vec<String>>> = OnceCell::new();

    fn mock_handler(category: &'static str, err: &anyhow::Error, loc: (&'static str, u32, u32)) {
        RESULT
            .get_or_init(|| Mutex::new(Vec::new()))
            .lock()
            .unwrap()
            .push(format!("{:?}, : {} : {}", loc, err, category));
    }

    #[test]
    fn test_soft_error() {
        // No logs without handler:
        let _ignore_hard_error = soft_error!(
            "test_unlogged_soft_error",
            anyhow::anyhow!("Should not be logged")
        );
        assert_eq!(
            0,
            RESULT
                .get_or_init(|| Mutex::new(Vec::new()))
                .lock()
                .unwrap()
                .len()
        );

        // Now set the handler and assert that we log
        super::initialize(box mock_handler);
        let _ignore_hard_error = soft_error!(
            "test_logged_soft_error",
            anyhow::anyhow!("Should be logged")
        );
        assert_eq!(
            Some(&format!(
                "({:?}, {}, 34), : Should be logged : test_logged_soft_error",
                file!(),
                line!() - 8,
            )),
            RESULT
                .get_or_init(|| Mutex::new(Vec::new()))
                .lock()
                .unwrap()
                .get(0)
        );
    }
}
