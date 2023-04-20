/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;

use crate::path_arg::PathArg;

/// Destination argument for clap that allows the user to specify the intention to either forward
/// data to a file with a provided path, or to an output stream
#[derive(Debug, Eq, PartialEq)]
pub enum OutputDestinationArg {
    Stream,
    Path(PathArg),
}

impl FromStr for OutputDestinationArg {
    type Err = <PathArg as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == Self::STREAM_TOKEN {
            Ok(OutputDestinationArg::Stream)
        } else {
            Ok(OutputDestinationArg::Path(PathArg::from_str(s)?))
        }
    }
}

impl OutputDestinationArg {
    /// Token used to specify stream forwarding
    const STREAM_TOKEN: &str = "-";
}
