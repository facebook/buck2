/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_data::error::ErrorTag;

use crate::Tier;

/// When there's no tag, but we want to put something in Scuba, we use this.
pub const ERROR_TAG_UNCLASSIFIED: &str = "UNCLASSIFIED";

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
        ErrorTag::ServerJemallocAssert => rank!(tier0),
        ErrorTag::ServerStackOverflow => rank!(tier0),
        ErrorTag::ServerPanicked => rank!(tier0),
        ErrorTag::ServerSegv => rank!(tier0),
        ErrorTag::ServerSigterm => rank!(environment),
        ErrorTag::InternalError => rank!(tier0),
        // FIXME(JakobDegen): Make this bad experience once that's available. Usually when this
        // happens, it's probably because the user tried to shut down with Ctrl+C and something
        // about that didn't work
        ErrorTag::InterruptedByDaemonShutdown => rank!(input),
        ErrorTag::DaemonWontDieFromKill => rank!(tier0),
        ErrorTag::DaemonIsBusy => rank!(input),
        ErrorTag::DaemonPreempted => rank!(input),
        ErrorTag::DaemonConnect => rank!(unspecified),
        ErrorTag::GrpcResponseMessageTooLarge => rank!(tier0),
        ErrorTag::ClientGrpc => rank!(tier0),
        ErrorTag::ConfigureAttr => rank!(input),
        ErrorTag::NoValidCerts => rank!(environment),
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
        ErrorTag::IoBrokenPipe => rank!(unspecified),
        ErrorTag::IoConnectionAborted => rank!(tier0),
        ErrorTag::IoNotConnected => rank!(input), // This typically means eden is not mounted
        ErrorTag::IoExecutableFileBusy => rank!(input),
        ErrorTag::IoStorageFull => rank!(input),
        ErrorTag::IoTimeout => rank!(tier0),
        ErrorTag::IoMaterializerFileBusy => rank!(environment),
        ErrorTag::IoWindowsSharingViolation => rank!(unspecified),
        ErrorTag::IoPermissionDenied => rank!(input),
        ErrorTag::IoNotFound => rank!(unspecified),
        ErrorTag::IoSource => rank!(unspecified),
        ErrorTag::IoSystem => rank!(unspecified),
        ErrorTag::IoEden => rank!(unspecified),
        ErrorTag::IoEdenConnectionError => rank!(unspecified),
        ErrorTag::IoEdenRequestError => rank!(unspecified),
        ErrorTag::IoEdenMountDoesNotExist => rank!(input),
        ErrorTag::IoEdenMountNotReady => rank!(tier0),
        // TODO(minglunli): Check how often Win32 Errors are actually hit, potentially do the same as POSIX
        ErrorTag::IoEdenWin32Error => rank!(tier0),
        ErrorTag::IoEdenHresultError => rank!(tier0),
        ErrorTag::IoEdenArgumentError => rank!(tier0),
        ErrorTag::IoEdenGenericError => rank!(tier0),
        ErrorTag::IoEdenMountGenerationChanged => rank!(tier0),
        ErrorTag::IoEdenJournalTruncated => rank!(tier0),
        ErrorTag::IoEdenCheckoutInProgress => rank!(input), // User switching branches during Eden operation
        ErrorTag::IoEdenOutOfDateParent => rank!(tier0),
        ErrorTag::IoEdenUnknownField => rank!(unspecified),
        ErrorTag::IoClientBrokenPipe => rank!(environment),
        ErrorTag::MaterializationError => rank!(unspecified),
        ErrorTag::ProjectMissingPath => rank!(input),
        ErrorTag::StarlarkFail => rank!(input),
        ErrorTag::StarlarkStackOverflow => rank!(input),
        ErrorTag::Visibility => rank!(input),
        ErrorTag::WatchmanTimeout => rank!(tier0),
        ErrorTag::WatchmanRequestError => rank!(tier0),
        ErrorTag::HttpServer => rank!(tier0),
        ErrorTag::HttpClient => rank!(input),
        ErrorTag::Http => rank!(unspecified),
        ErrorTag::DownloadFileHeadRequest => rank!(environment),
        ErrorTag::ServerStderrUnknown => rank!(unspecified),
        ErrorTag::ServerMemoryPressure => rank!(environment),
        ErrorTag::ServerStderrEmpty => rank!(unspecified),
        ErrorTag::Install => rank!(unspecified),
        ErrorTag::Analysis => rank!(input),
        ErrorTag::AnyActionExecution => rank!(unspecified),
        ErrorTag::AnyStarlarkEvaluation => rank!(unspecified),
        ErrorTag::UnusedDefaultTag => rank!(unspecified),
    }
}

pub trait ErrorLike {
    fn best_tag(&self) -> Option<ErrorTag>;
}

impl ErrorLike for buck2_data::ErrorReport {
    fn best_tag(&self) -> Option<ErrorTag> {
        best_tag(self.tags.iter().filter_map(|t| {
            // This should never be `None`, but with weak prost types,
            // it is safer to just ignore incorrect integers.
            ErrorTag::from_i32(*t)
        }))
    }
}

/// Pick the most interesting error by best tag.
pub fn best_error<'a>(
    tags: impl IntoIterator<Item = &'a buck2_data::ErrorReport>,
) -> Option<&'a buck2_data::ErrorReport> {
    tags.into_iter()
        .min_by_key(|e| e.best_tag().map(tag_rank).unwrap_or(u32::MAX))
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

#[cfg(test)]
mod tests {
    use buck2_data::error::ErrorTag;

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
}
