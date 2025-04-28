/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Error;

use buck2_error::buck2_error;
use winapi::shared::minwindef::BOOL;
use winapi::shared::minwindef::DWORD;
use winapi::shared::minwindef::FALSE;

pub(crate) fn result_bool(ret: BOOL) -> buck2_error::Result<()> {
    if ret == FALSE {
        Err(buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "{}",
            format!("{}", Error::last_os_error())
        ))
    } else {
        Ok(())
    }
}

pub(crate) fn result_dword(ret: DWORD) -> buck2_error::Result<()> {
    if ret == DWORD::MAX {
        Err(buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "{}",
            format!("{}", Error::last_os_error())
        ))
    } else {
        Ok(())
    }
}
