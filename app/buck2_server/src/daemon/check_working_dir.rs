/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use rand::distributions::Alphanumeric;
use rand::distributions::DistString;

/// Verify that our working directory is still here. We often run on Eden, and if Eden restarts
/// ungracefully, our working dir will become unreadable and we are just about done.
pub fn check_working_dir() -> buck2_error::Result<()> {
    use std::fs;
    use std::io;

    // Looks like we need to get a name that the OS isn't likely to have seen before for this to
    // work reliably.
    let name = Alphanumeric.sample_string(&mut rand::thread_rng(), 16);

    let err = match fs::metadata(name) {
        Ok(..) => return Ok(()),
        Err(e) => e,
    };

    if err.kind() == io::ErrorKind::NotConnected {
        let err = "Buck2 is running in an Eden mount but Eden restarted uncleanly. \
            This error is unrecoverable and you should restart Buck using `buck2 killall`.";
        return Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Environment,
            "{}",
            err
        ));
    }

    if err.kind() != io::ErrorKind::NotFound {
        tracing::warn!(
            "Buck2 is unable to read its current working directory: {}. Consider restarting",
            err
        );
    }

    Ok(())
}
