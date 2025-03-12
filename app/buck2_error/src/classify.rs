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

macro_rules! rank {
    ( $tier:ident ) => {
        match stringify!($tier) {
            "environment" => (Some(Tier::Environment), line!()),
            "tier0" => (Some(Tier::Tier0), line!()),
            "input" => (Some(Tier::Input), line!()),
            "unspecified" => (None, line!()),
            _ => unreachable!(),
        }
    };
}

/// Ordering determines tag rank, more interesting tags first
pub(crate) fn category_and_rank(tag: ErrorTag) -> (Option<Tier>, u32) {
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
        ErrorTag::InstallerEnvironment => rank!(environment),

        // Tier 0 errors
        ErrorTag::ServerJemallocAssert => rank!(tier0),
        ErrorTag::ServerStackOverflow => rank!(tier0),
        ErrorTag::ServerPanicked => rank!(tier0),
        ErrorTag::ServerSegv => rank!(tier0),
        ErrorTag::DaemonStateInitFailed => rank!(tier0),
        ErrorTag::DaemonConnect => rank!(tier0),
        ErrorTag::ServerStderrUnknown => rank!(tier0),
        ErrorTag::InternalError => rank!(tier0),
        ErrorTag::DaemonWontDieFromKill => rank!(tier0),
        ErrorTag::GrpcResponseMessageTooLarge => rank!(tier0),
        ErrorTag::ReUnknownTcode => rank!(tier0),
        ErrorTag::ReCancelled => rank!(tier0),
        ErrorTag::ReUnknown => rank!(tier0),
        ErrorTag::ReInvalidArgument => rank!(tier0),
        ErrorTag::ReDeadlineExceeded => rank!(tier0),
        ErrorTag::ReNotFound => rank!(tier0),
        ErrorTag::ReAlreadyExists => rank!(tier0),
        ErrorTag::RePermissionDenied => rank!(tier0),
        ErrorTag::ReResourceExhausted => rank!(tier0),
        ErrorTag::ReFailedPrecondition => rank!(tier0),
        ErrorTag::ReAborted => rank!(tier0),
        ErrorTag::ReOutOfRange => rank!(tier0),
        ErrorTag::ReUnimplemented => rank!(tier0),
        ErrorTag::ReInternal => rank!(tier0),
        ErrorTag::ReUnavailable => rank!(tier0),
        ErrorTag::ReDataLoss => rank!(tier0),
        ErrorTag::ReUnauthenticated => rank!(tier0),
        ErrorTag::IoConnectionAborted => rank!(tier0),
        ErrorTag::IoTimeout => rank!(tier0),
        ErrorTag::IoEdenMountNotReady => rank!(tier0),
        // TODO(minglunli): Check how often Win32 Errors are actually hit, potentially do the same as POSIX
        ErrorTag::IoEdenWin32Error => rank!(tier0),
        ErrorTag::IoEdenHresultError => rank!(tier0),
        ErrorTag::IoEdenArgumentError => rank!(tier0),
        ErrorTag::IoEdenGenericError => rank!(tier0),
        ErrorTag::IoEdenMountGenerationChanged => rank!(tier0),
        ErrorTag::IoEdenJournalTruncated => rank!(tier0),
        ErrorTag::IoEdenOutOfDateParent => rank!(tier0),
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
        ErrorTag::InstallerTier0 => rank!(tier0),

        ErrorTag::Environment => rank!(environment),
        ErrorTag::Tier0 => rank!(tier0),

        // Input errors
        // FIXME(JakobDegen): Make this bad experience once that's available. Usually when this
        // happens, it's probably because the user tried to shut down with Ctrl+C and something
        // about that didn't work
        ErrorTag::InterruptedByDaemonShutdown => rank!(input),
        ErrorTag::DaemonIsBusy => rank!(input),
        ErrorTag::DaemonPreempted => rank!(input),
        ErrorTag::ConfigureAttr => rank!(input),
        ErrorTag::DepOnlyIncompatible => rank!(input),
        ErrorTag::IoEdenCheckoutInProgress => rank!(input), // User switching branches during Eden operation
        ErrorTag::IoNotConnected => rank!(input), // This typically means eden is not mounted
        ErrorTag::IoExecutableFileBusy => rank!(input),
        ErrorTag::IoStorageFull => rank!(input),
        ErrorTag::IoPermissionDenied => rank!(input),
        ErrorTag::IoEdenMountDoesNotExist => rank!(input),
        ErrorTag::IoEdenFileNotFound => rank!(input), // user likely specified non-existing path
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
        ErrorTag::InstallerInput => rank!(input),
        ErrorTag::BuildDeadlineExpired => rank!(input),

        ErrorTag::Input => rank!(input),

        // Generic tags, these can represent:
        // - Tags not specific enough to determine infra vs user categorization.
        // - Tags not specific enough to usefully disambiguate category keys.
        // - Something that isn't actually an error.
        // - A phase of the build.
        ErrorTag::ClientGrpc => rank!(unspecified),
        ErrorTag::IoBrokenPipe => rank!(unspecified),
        ErrorTag::IoWindowsSharingViolation => rank!(unspecified),
        ErrorTag::IoNotFound => rank!(unspecified),
        ErrorTag::IoSource => rank!(unspecified),
        ErrorTag::IoSystem => rank!(unspecified),
        ErrorTag::IoEden => rank!(unspecified),
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
        // Build phases
        ErrorTag::Analysis => rank!(unspecified),
        ErrorTag::Install => rank!(unspecified),
        ErrorTag::AnyActionExecution => rank!(unspecified),
    }
}

/// Errors can be categorized by tags only if they have any non-generic tags.
pub fn tag_is_generic(tag: &ErrorTag) -> bool {
    if tag_is_hidden(tag) {
        return true;
    }
    category_and_rank(*tag).0.is_none()
}

/// Hidden tags only used internally, for categorization.
pub fn tag_is_hidden(tag: &ErrorTag) -> bool {
    match tag {
        ErrorTag::Tier0 => true,
        ErrorTag::Input => true,
        ErrorTag::Environment => true,
        ErrorTag::InstallerTier0 => true,
        ErrorTag::InstallerInput => true,
        ErrorTag::InstallerEnvironment => true,
        _ => false,
    }
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
            .map(|t| category_and_rank(t).0)
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
    category_and_rank(tag).1
}

/// Some tags are known to be either infrastructure or user errors.
pub(crate) fn error_tag_category(tag: ErrorTag) -> Option<Tier> {
    category_and_rank(tag).0
}

#[derive(derive_more::Display, Debug, PartialEq)]
pub enum ErrorSourceArea {
    Buck2,
    Eden,
    Re,
    Watchman,
    Starlark,
    TestExecutor,
    Installer,
}

pub fn source_area(tag: ErrorTag) -> ErrorSourceArea {
    let tag_name = tag.as_str_name();
    if tag_name.starts_with("IO_EDEN") {
        ErrorSourceArea::Eden
    } else if tag_name.starts_with("RE") {
        ErrorSourceArea::Re
    } else if tag_name.starts_with("WATCHMAN") {
        ErrorSourceArea::Watchman
    } else if tag_name.starts_with("STARLARK") {
        ErrorSourceArea::Starlark
    } else if tag == crate::ErrorTag::Tpx || tag == crate::ErrorTag::TestExecutor {
        ErrorSourceArea::TestExecutor
    } else if tag_name.starts_with("INSTALLER") {
        ErrorSourceArea::Installer
    } else {
        ErrorSourceArea::Buck2
    }
}

#[cfg(test)]
mod tests {
    use buck2_data::error::ErrorTag;
    use buck2_data::ErrorReport;

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
            source_area(ErrorTag::ServerStderrEmpty),
            ErrorSourceArea::Buck2
        );
        assert_eq!(source_area(ErrorTag::ReAborted), ErrorSourceArea::Re);
        assert_eq!(
            source_area(ErrorTag::WatchmanConnect),
            ErrorSourceArea::Watchman
        );
        assert_eq!(
            source_area(ErrorTag::IoEdenArgumentError),
            ErrorSourceArea::Eden
        );
        assert_eq!(
            source_area(ErrorTag::TestExecutor),
            ErrorSourceArea::TestExecutor
        );
    }
}
