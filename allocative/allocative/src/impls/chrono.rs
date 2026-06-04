/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![cfg(feature = "chrono")]

use chrono::DateTime;
use chrono::TimeDelta;
use chrono::TimeZone;

use crate::allocative_trait::Allocative;
use crate::visitor::Visitor;

impl<Tz: TimeZone> Allocative for DateTime<Tz> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        visitor.enter_self_sized::<Self>().exit();
    }
}

impl Allocative for TimeDelta {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        visitor.enter_self_sized::<Self>().exit();
    }
}

#[cfg(test)]
mod tests {
    use chrono::DateTime;
    use chrono::TimeDelta;
    use chrono::Utc;

    use crate::golden::golden_test;

    #[test]
    fn test_datetime() {
        golden_test!(&DateTime::<Utc>::UNIX_EPOCH);
    }

    #[test]
    fn test_timedelta() {
        golden_test!(&TimeDelta::zero());
    }
}
