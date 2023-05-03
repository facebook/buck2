/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Mutex;
use std::sync::MutexGuard;
use std::sync::Once;

use buck2_core::error::initialize;
use buck2_core::error::reset_soft_error_counters;
use buck2_core::error::StructuredErrorOptions;
use buck2_core::is_open_source;
use buck2_core::soft_error;

static RESULT: Mutex<Vec<String>> = Mutex::new(Vec::new());

fn mock_handler(
    category: &str,
    err: &anyhow::Error,
    loc: (&str, u32, u32),
    options: StructuredErrorOptions,
) {
    RESULT.lock().unwrap().push(format!(
        "{:?}, : {} : {} : {}",
        loc, err, category, options.quiet
    ));
}

fn test_init() -> MutexGuard<'static, ()> {
    // Tests in Rust can be executed concurrently, and these tests work with global state,
    // so use mutex to ensure we only run one test at a time.
    static TEST_MUTEX: Mutex<()> = Mutex::new(());
    let guard = TEST_MUTEX.lock().unwrap();

    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        initialize(Box::new(mock_handler)).unwrap();
    });

    RESULT.lock().unwrap().clear();

    guard
}

#[test]
fn test_soft_error() {
    if is_open_source() {
        return; // Errors are always hard in open source
    }
    let _guard = test_init();

    let before_error_line = line!();
    let _ignore_hard_error = soft_error!(
        "test_logged_soft_error",
        anyhow::anyhow!("Should be logged")
    );
    assert_eq!(
        Some(&format!(
            "({:?}, {}, 30), : Should be logged : test_logged_soft_error : false",
            file!(),
            before_error_line + 1,
        )),
        RESULT.lock().unwrap().get(0)
    );
}

#[test]
fn test_reset_counters() {
    if is_open_source() {
        return; // Errors are always hard in open source
    }
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
