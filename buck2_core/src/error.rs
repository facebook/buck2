use std::sync::atomic::{AtomicUsize, Ordering};

use once_cell::sync::OnceCell;

use crate::env_helper::EnvHelper;

type SoftErrorHandler =
    Box<dyn Fn(&anyhow::Error, (&'static str, u32, u32)) + Send + Sync + 'static>;

static HANDLER: OnceCell<SoftErrorHandler> = OnceCell::new();

/// Throw a "soft_error" i.e. one that is destined to become a hard error
/// in the near future. The macro lives in this crate to allow it be
/// made available everywhere. Calling programs are responsible for
/// calling initialize() to provide a handler for logging these soft_errors
#[macro_export]
macro_rules! soft_error(
    ($e:expr) => { {
        static COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        buck2_core::error::handle_soft_error($e, &COUNT, (file!(), line!(), column!()));
    } };
);

pub fn handle_soft_error(err: anyhow::Error, count: &AtomicUsize, loc: (&'static str, u32, u32)) {
    static HARD_ERROR: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_HARD_ERROR");

    // We want to limit each error to appearing at most 10 times in a build (no point spamming people)
    if count.fetch_add(1, Ordering::SeqCst) <= 10 {
        tracing::warn!("Important warning at {}:{}:{} {}", loc.0, loc.1, loc.2, err);
        if let Some(handler) = HANDLER.get() {
            handler(&err, loc);
        }
    }

    if HARD_ERROR.get().ok() == Some(&Some(true)) {
        // We can't anyhow::Error out of here because we don't return a Result.
        // We don't want to panic out of here in case that goes into our panic handler.
        // So cheat and drop an `exit` (this branch is only ever taken in our tests).
        eprintln!("Important warning caused exit due to $BUCK2_TEST_HARD_ERROR");
        std::process::exit(1);
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

    use anyhow::anyhow;
    use once_cell::sync::OnceCell;

    use crate as buck2_core;
    use crate::soft_error;

    static RESULT: OnceCell<Mutex<Vec<String>>> = OnceCell::new();

    fn mock_handler(err: &anyhow::Error, loc: (&'static str, u32, u32)) {
        RESULT
            .get_or_init(|| Mutex::new(Vec::new()))
            .lock()
            .unwrap()
            .push(format!("{:?}, : {}", loc, err));
    }

    #[test]
    fn test_soft_error() {
        // No logs without handler:
        soft_error!(anyhow!("Should not be logged"));
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
        soft_error!(anyhow!("Should be logged"));
        assert_eq!(
            Some(&format!(
                "({:?}, {}, 9), : Should be logged",
                file!(),
                line!() - 5,
            )),
            RESULT
                .get_or_init(|| Mutex::new(Vec::new()))
                .lock()
                .unwrap()
                .get(0)
        );
    }
}
