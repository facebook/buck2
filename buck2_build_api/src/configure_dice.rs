/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::dice::data::SetIoProvider;
use buck2_common::io::IoProvider;
use dice::cycles::DetectCycles;
use dice::Dice;

use crate::bxl::calculation::BxlCalculationDyn;

/// Utility to configure the dice globals.
/// One place to not forget to initialize something in all places.
pub fn configure_dice_for_buck(
    io: Arc<dyn IoProvider>,
    bxl: &'static dyn BxlCalculationDyn,
    detect_cycles: DetectCycles,
) -> Arc<Dice> {
    let mut dice = Dice::builder();
    dice.set_io_provider(io);
    dice.set(bxl);
    dice.build(detect_cycles)
}
