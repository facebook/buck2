/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::unstable_crash_request::CrashType;
use buck2_cli_proto::GenericResponse;
use buck2_cli_proto::UnstableCrashRequest;

pub(crate) fn crash(req: UnstableCrashRequest) -> buck2_error::Result<GenericResponse> {
    let crash_type = CrashType::try_from(req.crash_type).map_err(|_| {
        buck2_error::buck2_error!(buck2_error::ErrorTag::Tier0, "{}", "bad request")
    })?;
    match crash_type {
        CrashType::Panic => {
            panic!("explicitly requested panic (via unstable_crash)");
            #[allow(unreachable_code)]
            Ok(GenericResponse {})
        }
        CrashType::Abort => {
            // Crash with SIGABRT.
            // Should trigger folly signal handler to dump stack trace.
            // SIGSEGV,SIGTERM,SIGBUS,SIGILL,etc. should behave similarly.
            // https://fburl.com/code/ap385ats
            std::process::abort();
        }
    }
}
