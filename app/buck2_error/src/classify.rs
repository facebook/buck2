/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_data::error::ErrorTag;

/// When there's no tag, but we want to put something in Scuba, we use this.
pub const ERROR_TAG_UNCLASSIFIED: &str = "UNCLASSIFIED";

#[derive(
    allocative::Allocative,
    PartialEq,
    Eq,
    Copy,
    Clone,
    Debug,
    PartialOrd,
    Ord
)]
pub enum Tier {
    // Expected errors in inputs explicitly tracked by buck.
    Input,
    // Errors that may be triggered by issues with the host,
    // resource limits, non-explicit dependencies or potentially
    // ambiguous input errors.
    // These can be tracked but not eliminated.
    Environment,
    // Unexpected errors in buck2 or core dependencies.
    // It should be possible to eliminate these, in theory.
    Tier0,
}

struct TagMetadata {
    category: Option<Tier>,
    rank: u32,
    // If true and an error includes non-generic tags,
    // generic tags will be excluded from category key.
    generic: bool,
    // Hidden tags are only used to determine category,
    // they are excluded from both category keys and error_tags
    // reported externally.
    // These should be removed/avoided if possible.
    hidden: bool,
}

impl TagMetadata {
    fn generic(self, generic: bool) -> Self {
        Self { generic, ..self }
    }

    fn hidden(self) -> Self {
        Self {
            hidden: true,
            ..self
        }
    }
}

macro_rules! rank {
    ( $tier:ident ) => {
        match stringify!($tier) {
            "environment" => TagMetadata {
                category: Some(Tier::Environment),
                rank: line!(),
                generic: false,
                hidden: false,
            },
            "tier0" => TagMetadata {
                category: Some(Tier::Tier0),
                rank: line!(),
                generic: false,
                hidden: false,
            },
            "input" => TagMetadata {
                category: Some(Tier::Input),
                rank: line!(),
                generic: false,
                hidden: false,
            },
            "unspecified" => TagMetadata {
                category: None,
                rank: line!(),
                generic: true,
                hidden: false,
            },
            _ => unreachable!(),
        }
    };
}

#[derive(derive_more::Display, Debug, PartialEq)]
pub enum ErrorSourceArea {
    Buck2,
    Eden,
    Re,
    Watchman,
    TestExecutor,
    Installer,
}

pub trait ErrorTagExtra {
    fn source_area(&self) -> ErrorSourceArea;
}

impl ErrorTagExtra for ErrorTag {
    fn source_area(&self) -> ErrorSourceArea {
        let tag_name = self.as_str_name();
        if tag_name.starts_with("IO_EDEN") {
            ErrorSourceArea::Eden
        } else if tag_name.starts_with("RE") {
            ErrorSourceArea::Re
        } else if tag_name.starts_with("WATCHMAN") {
            ErrorSourceArea::Watchman
        } else if *self == crate::ErrorTag::Tpx || *self == crate::ErrorTag::TestExecutor {
            ErrorSourceArea::TestExecutor
        } else if tag_name.starts_with("INSTALLER") {
            ErrorSourceArea::Installer
        } else {
            ErrorSourceArea::Buck2
        }
    }
}

/// Ordering determines tag rank, more interesting tags first
fn tag_metadata(tag: ErrorTag) -> TagMetadata {
    match tag {
        // Environment errors
        ErrorTag::NoValidCerts => rank!(environment),
        ErrorTag::ServerSigterm => rank!(environment),
        ErrorTag::IoMaterializerFileBusy => rank!(environment),
        ErrorTag::IoClientBrokenPipe => rank!(environment),
        ErrorTag::IoReadOnlyFilesystem => rank!(environment),
        ErrorTag::WatchmanRootNotConnectedError => rank!(environment),
        ErrorTag::WatchmanCheckoutInProgress => rank!(environment),
        ErrorTag::ServerTransportError => rank!(environment),
        ErrorTag::ServerMemoryPressure => rank!(environment),
        // Daemon was likely SIGKILLed, otherwise it should have written something to stderr
        ErrorTag::ServerStderrEmpty => rank!(environment),
        // Note: This is only true internally due to buckwrapper
        ErrorTag::NoBuckRoot => rank!(environment),
        ErrorTag::InstallerEnvironment => rank!(environment).hidden(),
        ErrorTag::IoNotConnected => rank!(environment), // This typically means eden is not mounted
        // Typically due to poor network performance and large artifacts.
        ErrorTag::ReDeadlineExceeded => rank!(environment),

        // Tier 0 errors
        ErrorTag::ServerJemallocAssert => rank!(tier0),
        ErrorTag::ServerStackOverflow => rank!(tier0),
        ErrorTag::ServerPanicked => rank!(tier0),
        ErrorTag::ServerSegv => rank!(tier0),
        ErrorTag::ServerSigbus => rank!(tier0),
        ErrorTag::ServerSigabrt => rank!(tier0),
        ErrorTag::ServerStderrUnknown => rank!(tier0),
        ErrorTag::InternalError => rank!(tier0),
        ErrorTag::DaemonWontDieFromKill => rank!(tier0),
        ErrorTag::GrpcResponseMessageTooLarge => rank!(tier0),
        ErrorTag::ReClientCrash => rank!(tier0),
        ErrorTag::ReUnknownTcode => rank!(tier0),
        ErrorTag::ReCancelled => rank!(tier0),
        ErrorTag::ReUnknown => rank!(tier0),
        ErrorTag::ReInvalidArgument => rank!(tier0),
        ErrorTag::ReNotFound => rank!(tier0),
        ErrorTag::ReAlreadyExists => rank!(tier0),
        ErrorTag::RePermissionDenied => rank!(tier0),
        ErrorTag::ReResourceExhausted => rank!(tier0),
        ErrorTag::ReAborted => rank!(tier0),
        ErrorTag::ReOutOfRange => rank!(tier0),
        ErrorTag::ReUnimplemented => rank!(tier0),
        ErrorTag::ReInternal => rank!(tier0),
        ErrorTag::ReUnavailable => rank!(tier0),
        ErrorTag::ReDataLoss => rank!(tier0),
        ErrorTag::ReUnauthenticated => rank!(tier0),
        ErrorTag::ReCasArtifactWrongNumberOfInputs => rank!(tier0),
        ErrorTag::ReCasArtifactWrongNumberOfOutputs => rank!(tier0),
        ErrorTag::ReCasArtifactGetDigestExpirationError => rank!(tier0),
        ErrorTag::ReCasArtifactInvalidExpiration => rank!(tier0),
        ErrorTag::ReCasArtifactExpired => rank!(tier0),
        ErrorTag::ReInvalidGetCasResponse => rank!(tier0),

        ErrorTag::Clap => rank!(tier0),
        ErrorTag::Hex => rank!(tier0),
        ErrorTag::Hyper => rank!(tier0),
        ErrorTag::Nix => rank!(tier0),
        ErrorTag::Prost => rank!(tier0),
        ErrorTag::Regex => rank!(tier0),
        ErrorTag::RelativePath => rank!(tier0),
        ErrorTag::Rusqlite => rank!(tier0),
        ErrorTag::Tokio => rank!(tier0),
        ErrorTag::Tonic => rank!(tier0),
        ErrorTag::Uuid => rank!(tier0),
        ErrorTag::SerdeJson => rank!(tier0),
        ErrorTag::StdSlice => rank!(tier0),
        ErrorTag::StdTime => rank!(tier0),
        ErrorTag::StdInfallible => rank!(tier0),
        ErrorTag::StdStripPrefix => rank!(tier0),
        ErrorTag::ParseNum => rank!(tier0),
        ErrorTag::ParseBool => rank!(tier0),
        ErrorTag::IntConversion => rank!(tier0),
        ErrorTag::StringUtf8 => rank!(tier0),
        ErrorTag::StringConversion => rank!(tier0),
        ErrorTag::CstringNul => rank!(tier0),

        ErrorTag::CacheUploadFailed => rank!(tier0),
        ErrorTag::InstallIdMismatch => rank!(tier0),
        ErrorTag::SymlinkParentMissing => rank!(tier0),

        ErrorTag::WorkerInit => rank!(tier0),
        ErrorTag::WorkerDirectoryExists => rank!(tier0),
        ErrorTag::WorkerCancelled => rank!(tier0),

        ErrorTag::DispatcherUnavailable => rank!(tier0),
        ErrorTag::DaemonStatus => rank!(tier0),
        ErrorTag::DaemonRedirect => rank!(tier0),
        ErrorTag::ReExperimentName => rank!(tier0),
        ErrorTag::CsvParse => rank!(tier0),
        ErrorTag::CasBlobCountMismatch => rank!(tier0),
        ErrorTag::DownloadSizeMismatch => rank!(tier0),

        ErrorTag::DigestTtlMismatch => rank!(tier0),
        ErrorTag::DigestTtlInvalidResponse => rank!(tier0),

        ErrorTag::Bxl => rank!(tier0),
        ErrorTag::Certs => rank!(tier0),
        ErrorTag::LogCmd => rank!(tier0),
        ErrorTag::HealthCheck => rank!(tier0),
        ErrorTag::OfflineArchive => rank!(tier0),
        ErrorTag::Profile => rank!(tier0),
        ErrorTag::Lsp => rank!(tier0),
        ErrorTag::CleanStale => rank!(tier0),
        ErrorTag::Explain => rank!(tier0),
        ErrorTag::Interpreter => rank!(tier0),
        ErrorTag::StarlarkServer => rank!(tier0),
        ErrorTag::KillAll => rank!(tier0),

        ErrorTag::InvalidEvent => rank!(tier0),
        ErrorTag::InvalidDigest => rank!(tier0),
        ErrorTag::InvalidDuration => rank!(tier0),
        ErrorTag::InvalidAuthToken => rank!(tier0),
        ErrorTag::InvalidUsername => rank!(tier0),
        ErrorTag::InvalidAbsPath => rank!(tier0),
        ErrorTag::InvalidBuckOutPath => rank!(tier0),
        ErrorTag::InvalidErrorReport => rank!(tier0),

        ErrorTag::WindowsUnsupported => rank!(tier0),

        ErrorTag::LocalResourceSetup => rank!(tier0),
        ErrorTag::TestOrchestrator => rank!(tier0),
        ErrorTag::TestStatusInvalid => rank!(tier0),
        ErrorTag::TestStatus => rank!(tier0),

        ErrorTag::CleanOutputs => rank!(tier0),
        ErrorTag::Sapling => rank!(tier0),
        ErrorTag::CrashRequested => rank!(tier0),
        ErrorTag::CpuStats => rank!(tier0),
        ErrorTag::FailedToKill => rank!(tier0),
        ErrorTag::CopyOutputs => rank!(tier0),
        ErrorTag::LogFilter => rank!(tier0),
        ErrorTag::TestOnly => rank!(tier0),
        ErrorTag::Bail => rank!(tier0),

        ErrorTag::EventLogUpload => rank!(tier0),
        ErrorTag::EventLog => rank!(tier0),

        ErrorTag::MallocStats => rank!(tier0),
        ErrorTag::Mallctl => rank!(tier0),

        ErrorTag::SuperConsole => rank!(tier0),
        ErrorTag::SuperConsoleInvalidWhitespace => rank!(tier0),

        ErrorTag::IoConnectionAborted => rank!(tier0),
        ErrorTag::IoTimeout => rank!(tier0),
        ErrorTag::IoEdenMountNotReady => rank!(tier0),
        ErrorTag::IoEdenConfigError => rank!(tier0),
        ErrorTag::IoEdenVersionError => rank!(tier0),
        ErrorTag::IoEdenThriftError => rank!(tier0),
        // TODO(minglunli): Check how often Win32 Errors are actually hit, potentially do the same as POSIX
        ErrorTag::IoEdenWin32Error => rank!(tier0),
        ErrorTag::IoEdenHresultError => rank!(tier0),
        ErrorTag::IoEdenArgumentError => rank!(tier0),
        ErrorTag::IoEdenGenericError => rank!(tier0),
        ErrorTag::IoEdenMountGenerationChanged => rank!(tier0),
        ErrorTag::IoEdenJournalTruncated => rank!(tier0),
        ErrorTag::IoEdenOutOfDateParent => rank!(tier0),
        ErrorTag::IoEdenListMounts => rank!(tier0),
        ErrorTag::IoBlockingExecutor => rank!(tier0),
        ErrorTag::WatchmanClient => rank!(tier0),
        ErrorTag::WatchmanTimeout => rank!(tier0),
        ErrorTag::WatchmanConnectionError => rank!(tier0),
        ErrorTag::WatchmanConnectionLost => rank!(tier0),
        ErrorTag::WatchmanConnectionDiscovery => rank!(tier0),
        ErrorTag::WatchmanServerError => rank!(tier0),
        ErrorTag::WatchmanResponseError => rank!(tier0),
        ErrorTag::WatchmanMissingField => rank!(tier0),
        ErrorTag::WatchmanDeserialize => rank!(tier0),
        ErrorTag::WatchmanSerialize => rank!(tier0),
        ErrorTag::WatchmanConnect => rank!(tier0),
        ErrorTag::WatchmanRequestError => rank!(tier0),
        ErrorTag::NotifyWatcher => rank!(tier0),
        ErrorTag::HttpServer => rank!(tier0),
        ErrorTag::StarlarkInternal => rank!(tier0),
        ErrorTag::ActionMismatchedOutputs => rank!(tier0),
        ErrorTag::DiceDuplicatedChange => rank!(tier0),
        ErrorTag::DiceChangedToInvalid => rank!(tier0),
        ErrorTag::DiceInjectedKeyGotInvalidation => rank!(tier0),
        ErrorTag::DiceCancelled => rank!(tier0),
        ErrorTag::DiceUnexpectedCycleGuardType => rank!(tier0),
        ErrorTag::DiceDuplicateActivationData => rank!(tier0),
        ErrorTag::InstallerUnknown => rank!(tier0),
        ErrorTag::InstallerTier0 => rank!(tier0).hidden(),

        ErrorTag::Environment => rank!(environment).hidden(),
        ErrorTag::Tier0 => rank!(tier0).hidden(),

        // Input errors
        ErrorTag::ReFailedPrecondition => rank!(input),
        // FIXME(JakobDegen): Make this bad experience once that's available. Usually when this
        // happens, it's probably because the user tried to shut down with Ctrl+C and something
        // about that didn't work
        ErrorTag::InterruptedByDaemonShutdown => rank!(input),
        ErrorTag::DaemonIsBusy => rank!(input),
        ErrorTag::DaemonPreempted => rank!(input),
        ErrorTag::ConfigureAttr => rank!(input),
        ErrorTag::DepOnlyIncompatible => rank!(input),
        ErrorTag::TargetIncompatible => rank!(input),
        ErrorTag::IoEdenCheckoutInProgress => rank!(input), // User switching branches during Eden operation
        ErrorTag::IoExecutableFileBusy => rank!(input),
        ErrorTag::IoStorageFull => rank!(input),
        ErrorTag::IoPermissionDenied => rank!(input),
        ErrorTag::IoEdenMountDoesNotExist => rank!(input),
        ErrorTag::IoEdenFileNotFound => rank!(input), // user likely specified non-existing path
        ErrorTag::MissingTarget => rank!(input),
        ErrorTag::ActionMissingOutputs => rank!(input),
        ErrorTag::ActionWrongOutputType => rank!(input),
        ErrorTag::ActionCommandFailure => rank!(input),
        ErrorTag::ProjectMissingPath => rank!(input),
        ErrorTag::StarlarkFail => rank!(input),
        ErrorTag::StarlarkStackOverflow => rank!(input),
        ErrorTag::StarlarkValue => rank!(input),
        ErrorTag::StarlarkFunction => rank!(input),
        ErrorTag::StarlarkScope => rank!(input),
        ErrorTag::StarlarkParser => rank!(input),
        ErrorTag::StarlarkNativeInput => rank!(input),
        ErrorTag::Visibility => rank!(input),
        ErrorTag::HttpClient => rank!(input),
        ErrorTag::TestDeadlineExpired => rank!(input),
        ErrorTag::Unimplemented => rank!(input),
        ErrorTag::InstallerInput => rank!(input).hidden(),
        ErrorTag::BuildDeadlineExpired => rank!(input),

        ErrorTag::Input => rank!(input).hidden(),

        // Tags with unspecified category, these can represent:
        // - Tags not specific enough to determine infra vs user categorization.
        // - Tags not specific enough to usefully disambiguate category keys.
        // - Something that isn't actually an error.
        // - A phase of the build.
        // By default these are generic (excluded from category keys)
        ErrorTag::ClientGrpc => rank!(unspecified),
        ErrorTag::CompatibilityError => rank!(unspecified),
        ErrorTag::IoBrokenPipe => rank!(unspecified),
        ErrorTag::IoWindowsSharingViolation => rank!(unspecified),
        ErrorTag::IoNotFound => rank!(unspecified),
        ErrorTag::IoInputOutputError => rank!(unspecified),
        ErrorTag::IoSource => rank!(unspecified),
        ErrorTag::IoSystem => rank!(unspecified),
        ErrorTag::IoEden => rank!(unspecified).generic(false),
        ErrorTag::IoEdenConnectionError => rank!(unspecified),
        ErrorTag::IoEdenRequestError => rank!(unspecified),
        ErrorTag::IoEdenUnknownField => rank!(unspecified),
        ErrorTag::MaterializationError => rank!(unspecified),
        ErrorTag::CleanInterrupt => rank!(unspecified),
        ErrorTag::Tpx => rank!(unspecified),
        ErrorTag::TestExecutor => rank!(unspecified),
        ErrorTag::Http => rank!(unspecified),
        ErrorTag::DownloadFileHeadRequest => rank!(unspecified),
        ErrorTag::StarlarkError => rank!(unspecified),
        ErrorTag::UnexpectedNone => rank!(unspecified),
        ErrorTag::UnusedDefaultTag => rank!(unspecified),
        ErrorTag::BuildSketchError => rank!(unspecified),
        // Build phases
        ErrorTag::DaemonStateInitFailed => rank!(unspecified),
        ErrorTag::DaemonConnect => rank!(unspecified),
        ErrorTag::Analysis => rank!(unspecified),
        ErrorTag::Install => rank!(unspecified),
        ErrorTag::AnyActionExecution => rank!(unspecified),
    }
}

/// Errors can be categorized by tags only if they have any non-generic tags.
pub fn tag_is_generic(tag: &ErrorTag) -> bool {
    let metadata = tag_metadata(*tag);
    metadata.generic || metadata.hidden
}

/// Hidden tags only used internally, for categorization.
pub fn tag_is_hidden(tag: &ErrorTag) -> bool {
    tag_metadata(*tag).hidden
}

pub trait ErrorLike {
    fn best_tag(&self) -> Option<ErrorTag>;

    fn error_rank(&self) -> u32;

    fn category(&self) -> Tier;
}

impl ErrorLike for buck2_data::ErrorReport {
    fn best_tag(&self) -> Option<ErrorTag> {
        best_tag(self.tags.iter().filter_map(|t| {
            // This should never be `None`, but with weak prost types,
            // it is safer to just ignore incorrect integers.
            ErrorTag::try_from(*t).ok()
        }))
    }

    fn error_rank(self: &buck2_data::ErrorReport) -> u32 {
        self.best_tag().map(tag_rank).unwrap_or(u32::MAX)
    }

    fn category(&self) -> Tier {
        self.best_tag()
            .map(|t| tag_metadata(t).category)
            .flatten()
            .unwrap_or(Tier::Tier0)
    }
}

/// Pick the most interesting error by best tag.
pub fn best_error<'a>(
    tags: impl IntoIterator<Item = &'a buck2_data::ErrorReport>,
) -> Option<&'a buck2_data::ErrorReport> {
    tags.into_iter().min_by_key(|e| e.error_rank())
}

/// Pick the most interesting tag from a list of tags.
pub fn best_tag(tags: impl IntoIterator<Item = ErrorTag>) -> Option<ErrorTag> {
    tags.into_iter().min_by_key(|t| tag_rank(*t))
}

/// Tag rank: smaller is more interesting.
fn tag_rank(tag: ErrorTag) -> u32 {
    tag_metadata(tag).rank
}

/// Some tags are known to be either infrastructure or user errors.
pub(crate) fn error_tag_category(tag: ErrorTag) -> Option<Tier> {
    tag_metadata(tag).category
}

// Buck2 is the fallback/default source area, use the first non-buck2 source area.
pub fn source_area(tags: impl IntoIterator<Item = ErrorTag>) -> ErrorSourceArea {
    tags.into_iter()
        .find_map(|tag| {
            let area = tag.source_area();
            if area != ErrorSourceArea::Buck2 {
                Some(area)
            } else {
                None
            }
        })
        .unwrap_or(ErrorSourceArea::Buck2)
}

#[cfg(test)]
mod tests {
    use buck2_data::ErrorReport;
    use buck2_data::error::ErrorTag;

    use super::*;
    use crate::classify::best_tag;

    #[test]
    fn test_best_tag() {
        assert_eq!(
            Some(ErrorTag::ServerPanicked),
            best_tag([ErrorTag::ServerPanicked, ErrorTag::WatchmanTimeout])
        )
    }

    #[test]
    fn test_rank() {
        assert!(
            super::tag_rank(ErrorTag::ServerJemallocAssert)
                < super::tag_rank(ErrorTag::UnusedDefaultTag)
        )
    }

    #[test]
    fn test_first_error_best_error() {
        let mut errors = vec![
            ErrorReport {
                tags: vec![ErrorTag::Input as i32],
                ..ErrorReport::default()
            },
            ErrorReport {
                tags: vec![ErrorTag::Tier0 as i32],
                ..ErrorReport::default()
            },
        ];

        let best_error = best_error(&errors).unwrap().clone();
        assert_eq!(best_error.category(), Tier::Tier0);

        // Test that first error in sorted list is equivalent to best_error.
        errors.sort_by_key(|e| e.error_rank());
        assert_eq!(errors.first(), Some(&best_error));
    }

    #[test]
    fn test_default_is_infra() {
        let errors = vec![ErrorReport {
            tags: vec![ErrorTag::UnusedDefaultTag as i32],
            ..ErrorReport::default()
        }];

        assert_eq!(best_error(&errors).map(|e| e.category()), Some(Tier::Tier0));
    }

    #[test]
    fn test_ranked_infra() {
        let errors = vec![
            ErrorReport {
                tags: vec![ErrorTag::ServerJemallocAssert as i32],
                ..ErrorReport::default()
            },
            ErrorReport {
                tags: vec![ErrorTag::Tier0 as i32],
                ..ErrorReport::default()
            },
        ];

        assert_eq!(
            best_error(&errors).map(|e| e.tags.clone()),
            Some(vec![ErrorTag::ServerJemallocAssert as i32]),
        );
    }

    #[test]
    fn test_ranked_tags() {
        let errors = vec![ErrorReport {
            tags: vec![
                ErrorTag::ServerStderrEmpty as i32,
                ErrorTag::ClientGrpc as i32,
            ],
            ..ErrorReport::default()
        }];

        assert_eq!(
            best_error(&errors).map(|e| e.category()),
            Some(Tier::Environment)
        );
    }

    #[test]
    fn test_source_area() {
        assert_eq!(
            ErrorTag::ServerStderrEmpty.source_area(),
            ErrorSourceArea::Buck2
        );
        assert_eq!(ErrorTag::ReAborted.source_area(), ErrorSourceArea::Re);
        assert_eq!(
            ErrorTag::WatchmanConnect.source_area(),
            ErrorSourceArea::Watchman
        );
        assert_eq!(
            ErrorTag::IoEdenArgumentError.source_area(),
            ErrorSourceArea::Eden
        );
        assert_eq!(
            ErrorTag::TestExecutor.source_area(),
            ErrorSourceArea::TestExecutor
        );
    }
}
