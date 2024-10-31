/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_data::error::ErrorTag;
use buck2_data::error::ErrorTier;

use crate::Tier;

/// When there's no tag, but we want to put something in Scuba, we use this.
pub const ERROR_TAG_UNCLASSIFIED: &str = "UNCLASSIFIED";

fn tier_rank(tier: Option<Tier>) -> u32 {
    match tier {
        Some(tier) => match tier {
            Tier::Environment => 10000,
            Tier::Tier0 => 10001,
            Tier::Input => 20000,
        },
        None => 30000,
    }
}

macro_rules! rank {
    ( $tier:ident ) => {
        match stringify!($tier) {
            "environment" => (Some(Tier::Environment), line!()),
            "tier0" => (Some(Tier::Tier0), line!()),
            "input" => (Some(Tier::Input), tier_rank(Some(Tier::Tier0)) + line!()),
            "unspecified" => (None, tier_rank(Some(Tier::Input)) + line!()),
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
        ErrorTag::WatchmanRootNotConnectedError => rank!(environment),
        ErrorTag::DownloadFileHeadRequest => rank!(environment),
        ErrorTag::ServerMemoryPressure => rank!(environment),
        // Daemon was likely SIGKILLed, otherwise it should have written something to stderr
        ErrorTag::ServerStderrEmpty => rank!(environment),

        // Tier 0 errors
        ErrorTag::ServerJemallocAssert => rank!(tier0),
        ErrorTag::ServerStackOverflow => rank!(tier0),
        ErrorTag::ServerPanicked => rank!(tier0),
        ErrorTag::ServerSegv => rank!(tier0),
        ErrorTag::DaemonConnect => rank!(tier0),
        ErrorTag::ServerStderrUnknown => rank!(tier0),
        ErrorTag::InternalError => rank!(tier0),
        ErrorTag::DaemonWontDieFromKill => rank!(tier0),
        ErrorTag::GrpcResponseMessageTooLarge => rank!(tier0),
        ErrorTag::ClientGrpc => rank!(tier0),
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

        // Input errors
        // FIXME(JakobDegen): Make this bad experience once that's available. Usually when this
        // happens, it's probably because the user tried to shut down with Ctrl+C and something
        // about that didn't work
        ErrorTag::InterruptedByDaemonShutdown => rank!(input),
        ErrorTag::DaemonIsBusy => rank!(input),
        ErrorTag::DaemonPreempted => rank!(input),
        ErrorTag::ConfigureAttr => rank!(input),
        ErrorTag::IoEdenCheckoutInProgress => rank!(input), // User switching branches during Eden operation
        ErrorTag::IoNotConnected => rank!(input), // This typically means eden is not mounted
        ErrorTag::IoExecutableFileBusy => rank!(input),
        ErrorTag::IoStorageFull => rank!(input),
        ErrorTag::IoPermissionDenied => rank!(input),
        ErrorTag::IoEdenMountDoesNotExist => rank!(input),
        ErrorTag::ProjectMissingPath => rank!(input),
        ErrorTag::StarlarkFail => rank!(input),
        ErrorTag::StarlarkStackOverflow => rank!(input),
        ErrorTag::Visibility => rank!(input),
        ErrorTag::HttpClient => rank!(input),
        ErrorTag::Analysis => rank!(input),
        ErrorTag::TestDeadlineExpired => rank!(input),

        // Unspecified errors
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
        ErrorTag::Http => rank!(unspecified),
        ErrorTag::Install => rank!(unspecified),
        ErrorTag::AnyActionExecution => rank!(unspecified),
        ErrorTag::AnyStarlarkEvaluation => rank!(unspecified),
        ErrorTag::UnusedDefaultTag => rank!(unspecified),
    }
}

pub trait ErrorLike {
    fn best_tag(&self) -> Option<ErrorTag>;

    fn get_tier(&self) -> Option<Tier>;

    fn error_rank(&self) -> u32;

    fn category(&self) -> String;
}

const TIER0: &str = "INFRA";
const ENVIRONMENT: &str = "ENVIRONMENT";
const INPUT: &str = "USER";

impl ErrorLike for buck2_data::ErrorReport {
    fn best_tag(&self) -> Option<ErrorTag> {
        best_tag(self.tags.iter().filter_map(|t| {
            // This should never be `None`, but with weak prost types,
            // it is safer to just ignore incorrect integers.
            ErrorTag::from_i32(*t)
        }))
    }

    fn get_tier(&self) -> Option<Tier> {
        self.tier
            .map(|tier| match ErrorTier::from_i32(tier) {
                Some(tier) => match tier {
                    ErrorTier::Tier0 => Some(Tier::Tier0),
                    ErrorTier::Environment => Some(Tier::Environment),
                    ErrorTier::Input => Some(Tier::Input),
                    ErrorTier::UnusedDefaultCategory => None,
                },
                None => None,
            })
            .flatten()
    }

    fn error_rank(self: &buck2_data::ErrorReport) -> u32 {
        let tag_rank = self.best_tag().map(tag_rank).unwrap_or(u32::MAX);
        let tier_rank = tier_rank(self.get_tier());

        std::cmp::min(tag_rank, tier_rank)
    }

    fn category(&self) -> String {
        let tier = self
            .best_tag()
            .map(|t| category_and_rank(t).0)
            .flatten()
            .or(self.get_tier())
            .unwrap_or(Tier::Tier0);

        match tier {
            Tier::Tier0 => TIER0.to_owned(),
            Tier::Environment => ENVIRONMENT.to_owned(),
            Tier::Input => INPUT.to_owned(),
        }
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

#[cfg(test)]
mod tests {
    use buck2_data::error::ErrorTag;
    use buck2_data::error::ErrorTier;
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
    fn test_user_and_infra() {
        let errors = vec![
            ErrorReport {
                tier: Some(ErrorTier::Input as i32),
                ..ErrorReport::default()
            },
            ErrorReport {
                tier: Some(ErrorTier::Tier0 as i32),
                ..ErrorReport::default()
            },
        ];

        assert_eq!(
            best_error(&errors).map(|e| e.category()),
            Some(TIER0.to_owned())
        );
    }

    #[test]
    fn test_default_is_infra() {
        let errors = vec![ErrorReport {
            tier: Some(ErrorTier::UnusedDefaultCategory as i32),
            ..ErrorReport::default()
        }];

        assert_eq!(
            best_error(&errors).map(|e| e.category()),
            Some(TIER0.to_owned())
        );
    }

    #[test]
    fn test_ranked_infra() {
        let errors = vec![
            ErrorReport {
                tags: vec![ErrorTag::ServerJemallocAssert as i32],
                ..ErrorReport::default()
            },
            ErrorReport {
                tier: Some(ErrorTier::Tier0 as i32),
                ..ErrorReport::default()
            },
        ];

        assert_eq!(
            best_error(&errors).map(|e| e.tags.clone()),
            Some(vec![ErrorTag::ServerJemallocAssert as i32]),
        );
    }

    #[test]
    fn test_tag_overrides_tier() {
        let errors = vec![ErrorReport {
            tier: Some(ErrorTier::Tier0 as i32),
            tags: vec![ErrorTag::StarlarkFail as i32],
            ..ErrorReport::default()
        }];

        assert_eq!(
            best_error(&errors).map(|e| e.category()),
            Some(INPUT.to_owned())
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
            Some(ENVIRONMENT.to_owned())
        );
    }
}
