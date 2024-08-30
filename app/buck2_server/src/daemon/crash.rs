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

pub(crate) fn crash(req: UnstableCrashRequest) -> anyhow::Result<GenericResponse> {
    let crash_type = CrashType::from_i32(req.crash_type).ok_or(anyhow::anyhow!("bad request"))?;
    match crash_type {
        CrashType::Panic => {
            panic!("explicitly requested panic (via unstable_crash)");
            #[allow(unreachable_code)]
            Ok(GenericResponse {})
        }
        CrashType::Segfault => {
            unsafe {
                std::ptr::null_mut::<&'static str>()
                    .write("Explicitly requested segfault (via `segfault`)")
            };
            unreachable!()
        }
    }
}
