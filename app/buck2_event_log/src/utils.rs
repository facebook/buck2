/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::str::FromStr;
use std::time::SystemTime;

use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_error::BuckErrorContext;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use itertools::Itertools;

#[derive(Debug, buck2_error::Error)]
pub(crate) enum EventLogErrors {
    #[error(
        "Trying to write to logfile that hasn't been opened yet - this is an internal error, please report. Unwritten event: {serialized_event}"
    )]
    #[buck2(tag = EventLogNotOpen)]
    LogNotOpen { serialized_event: String },
    #[error("Reached End of File before reading BuckEvent in log `{0}`")]
    #[buck2(tag = EventLogEof)]
    EndOfFile(String),
    #[error("No event log available for {idx}th last command (have latest {num_logfiles})")]
    #[buck2(tag = EventLogIndexOutOfBounds)]
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

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
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

    #[error("Event log at path {} has no uuid in its filename", .0.display())]
    NoUuidInFilename(AbsPathBuf),
}

fn display_valid_extensions() -> String {
    let mut exts = KNOWN_ENCODINGS
        .iter()
        .flat_map(|encoding| encoding.extensions);
    exts.join(", ")
}

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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Invocation {
    pub command_line_args: Vec<String>,
    /// Command line args with expanded `@` args.
    pub expanded_command_line_args: Vec<String>,
    /// This is `String` not `AbsPathBuf` because event log is cross-platform
    /// and `AbsPathBuf` is not.
    pub working_dir: String,
    pub trace_id: TraceId,
    pub start_time: SystemTime,
}

impl Invocation {
    pub fn display_command_line(&self) -> String {
        shlex::try_join(self.command_line_args.iter().map(|e| e.as_str()))
            .expect("Null byte unexpected")
    }

    pub fn display_expanded_command_line(&self) -> String {
        shlex::try_join(self.expanded_command_line_args.iter().map(|e| e.as_str()))
            .expect("Null byte unexpected")
    }

    pub(crate) fn parse_json_line(json: &str) -> buck2_error::Result<Invocation> {
        let i = serde_json::from_str::<buck2_data::Invocation>(json)
            .with_buck_error_context(|| format!("Invalid header: {}", json.trim_end()))?;
        Ok(Invocation::from_proto(i))
    }

    pub fn to_proto(self) -> buck2_data::Invocation {
        buck2_data::Invocation {
            command_line_args: self.command_line_args.clone(),
            expanded_command_line_args: self.expanded_command_line_args.clone(),
            working_dir: self.working_dir.clone(),
            trace_id: Some(self.trace_id.to_string()),
            start_time: Some(self.start_time.into()),
        }
    }

    pub(crate) fn from_proto(proto: buck2_data::Invocation) -> Self {
        Invocation {
            command_line_args: proto.command_line_args,
            expanded_command_line_args: proto.expanded_command_line_args,
            working_dir: proto.working_dir,
            trace_id: proto
                .trace_id
                .and_then(|s| TraceId::from_str(&s).ok())
                .unwrap_or(TraceId::null()),
            start_time: proto
                .start_time
                .and_then(|t| t.try_into().ok())
                .unwrap_or(SystemTime::UNIX_EPOCH),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use std::time::SystemTime;

    use buck2_wrapper_common::invocation_id::TraceId;

    use crate::utils::Invocation;

    #[test]
    fn test_parse_json_line() {
        // Make sure serialization format is backwards compatible.
        let line = r#"{"command_line_args":["/some/path/buck2","test","@//mode/mac","app/..."],"working_dir":"/Users/nga/dir45","trace_id":"281d1c16-8930-40cd-8fc1-7d71355c20f5"}"#;
        let line = Invocation::parse_json_line(line).unwrap();
        let expected = Invocation {
            command_line_args: vec![
                "/some/path/buck2".to_owned(),
                "test".to_owned(),
                "@//mode/mac".to_owned(),
                "app/...".to_owned(),
            ],
            working_dir: "/Users/nga/dir45".to_owned(),
            expanded_command_line_args: Vec::new(),
            trace_id: TraceId::from_str("281d1c16-8930-40cd-8fc1-7d71355c20f5").unwrap(),
            start_time: SystemTime::UNIX_EPOCH,
        };
        assert_eq!(expected, line);
    }
}
