/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use itertools::Itertools;
use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;

#[derive(Error, Debug)]
pub(crate) enum EventLogErrors {
    #[error(
        "Trying to write to logfile that hasn't been opened yet - this is an internal error, please report. Unwritten event: {serialized_event}"
    )]
    LogNotOpen { serialized_event: String },

    #[error("Reached End of File before reading BuckEvent in log `{0}`")]
    EndOfFile(String),
    #[error("No event log available for {idx}th last command (have latest {num_logfiles})")]
    RecentIndexOutOfBounds { idx: usize, num_logfiles: usize },
}

#[derive(Copy, Clone, Dupe, Debug)]
pub struct Encoding {
    pub(crate) mode: LogMode,
    pub(crate) compression: Compression,
    /// List of extensions used to detect file type.
    ///
    /// The first extension is the default one, used when writing a file.
    pub extensions: &'static [&'static str],
}

impl Encoding {
    pub(crate) const JSON: Encoding = Encoding {
        mode: LogMode::Json,
        compression: Compression::None,
        extensions: &[".json-lines"],
    };

    pub(crate) const JSON_GZIP: Encoding = Encoding {
        mode: LogMode::Json,
        compression: Compression::Gzip,
        extensions: &[".json-lines.gz"],
    };

    pub(crate) const JSON_ZSTD: Encoding = Encoding {
        mode: LogMode::Json,
        compression: Compression::Zstd,
        extensions: &[".json-lines.zst"],
    };

    pub(crate) const PROTO: Encoding = Encoding {
        mode: LogMode::Protobuf,
        compression: Compression::None,
        extensions: &[".pb", ".proto"],
    };

    pub(crate) const PROTO_GZIP: Encoding = Encoding {
        mode: LogMode::Protobuf,
        compression: Compression::Gzip,
        extensions: &[".pb.gz", ".proto.gz"],
    };

    pub const PROTO_ZSTD: Encoding = Encoding {
        mode: LogMode::Protobuf,
        compression: Compression::Zstd,
        extensions: &[".pb.zst"],
    };
}

pub(crate) const KNOWN_ENCODINGS: &[Encoding] = &[
    // Don't forget to update these lists when this is updated:
    // * https://fburl.com/code/zgdxtryb
    // * https://fburl.com/code/antguytj
    Encoding::JSON_GZIP,
    Encoding::JSON,
    Encoding::JSON_ZSTD,
    Encoding::PROTO,
    Encoding::PROTO_GZIP,
    Encoding::PROTO_ZSTD,
];

#[derive(Error, Debug)]
pub(crate) enum EventLogInferenceError {
    #[error("Event log at path {} has no filename", .0.display())]
    NoFilename(AbsPathBuf),

    #[error("Event log at path {} has a non-utf-8 filename", .0.display())]
    InvalidFilename(AbsPathBuf),

    #[error(
        "Event log at path {} has an extension that was not recognized. Valid extensions are: {}.",
        .0.display(), display_valid_extensions()
    )]
    InvalidExtension(AbsPathBuf),
}

fn display_valid_extensions() -> String {
    let mut exts = KNOWN_ENCODINGS
        .iter()
        .flat_map(|encoding| encoding.extensions);
    exts.join(", ")
}

pub(crate) struct NoInference(pub(crate) AbsPathBuf);

#[derive(Copy, Clone, Dupe, Debug, PartialEq, Eq)]
pub(crate) enum LogMode {
    Json,
    Protobuf,
}

#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) enum Compression {
    None,
    Gzip,
    Zstd,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Invocation {
    pub command_line_args: Vec<String>,
    /// This is `String` not `AbsPathBuf` because event log is cross-platform
    /// and `AbsPathBuf` is not.
    pub working_dir: String,
    #[serde(default = "TraceId::null")]
    pub trace_id: TraceId,
}

impl Invocation {
    pub fn display_command_line(&self) -> String {
        shlex::join(self.command_line_args.iter().map(|e| e.as_str()))
    }
}
