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

/// Pick the most interesting tag from a list of tags.
pub fn best_tag(tags: impl IntoIterator<Item = ErrorTag>) -> Option<ErrorTag> {
    tags.into_iter().min_by_key(|t| tag_rank(*t))
}

/// Tag rank: smaller is more interesting.
fn tag_rank(tag: ErrorTag) -> u32 {
    match tag {
        ErrorTag::ServerJemallocAssert => line!(),
        ErrorTag::ServerStackOverflow => line!(),
        ErrorTag::ServerPanicked => line!(),
        ErrorTag::ServerSegv => line!(),
        ErrorTag::InternalError => line!(),
        ErrorTag::InterruptedByDaemonShutdown => line!(),
        ErrorTag::DaemonWontDieFromKill => line!(),
        ErrorTag::DaemonIsBusy => line!(),
        ErrorTag::DaemonPreempted => line!(),
        ErrorTag::DaemonConnect => line!(),
        ErrorTag::GrpcResponseMessageTooLarge => line!(),
        ErrorTag::ClientGrpc => line!(),
        ErrorTag::NoValidCerts => line!(),
        ErrorTag::IoBrokenPipe => line!(),
        ErrorTag::IoConnectionAborted => line!(),
        ErrorTag::IoNotConnected => line!(),
        ErrorTag::IoExecutableFileBusy => line!(),
        ErrorTag::IoStorageFull => line!(),
        ErrorTag::IoTimeout => line!(),
        ErrorTag::IoPermissionDenied => line!(),
        ErrorTag::IoNotFound => line!(),
        ErrorTag::IoSource => line!(),
        ErrorTag::IoSystem => line!(),
        ErrorTag::IoEden => line!(),
        ErrorTag::IoEdenConnectionError => line!(),
        ErrorTag::IoEdenRequestError => line!(),
        ErrorTag::IoEdenMountDoesNotExist => line!(),
        ErrorTag::IoEdenMountNotReady => line!(),
        ErrorTag::IoEdenWin32Error => line!(),
        ErrorTag::IoEdenHresultError => line!(),
        ErrorTag::IoEdenArgumentError => line!(),
        ErrorTag::IoEdenGenericError => line!(),
        ErrorTag::IoEdenMountGenerationChanged => line!(),
        ErrorTag::IoEdenJournalTruncated => line!(),
        ErrorTag::IoEdenCheckoutInProgress => line!(),
        ErrorTag::IoEdenOutOfDateParent => line!(),
        ErrorTag::IoEdenUnknownField => line!(),
        ErrorTag::IoClientBrokenPipe => line!(),
        ErrorTag::ProjectMissingPath => line!(),
        ErrorTag::StarlarkFail => line!(),
        ErrorTag::StarlarkStackOverflow => line!(),
        ErrorTag::Visibility => line!(),
        ErrorTag::WatchmanTimeout => line!(),
        ErrorTag::HttpServer => line!(),
        ErrorTag::HttpClient => line!(),
        ErrorTag::Http => line!(),
        ErrorTag::ServerStderrUnknown => line!(),
        ErrorTag::ServerStderrEmpty => line!(),
        ErrorTag::Install => line!(),
        ErrorTag::Analysis => line!(),
        ErrorTag::AnyActionExecution => line!(),
        ErrorTag::AnyStarlarkEvaluation => line!(),
        ErrorTag::UnusedDefaultTag => line!(),
    }
}

/// Some tags are known to be either infrastructure or user errors.
pub(crate) fn error_tag_category(tag: ErrorTag) -> Option<Tier> {
    match tag {
        ErrorTag::ServerJemallocAssert => Some(Tier::Tier0),
        ErrorTag::ServerStackOverflow => Some(Tier::Tier0),
        ErrorTag::ServerPanicked => Some(Tier::Tier0),
        ErrorTag::ServerSegv => Some(Tier::Tier0),
        ErrorTag::DaemonWontDieFromKill => Some(Tier::Tier0),
        ErrorTag::DaemonConnect => None,
        ErrorTag::DaemonIsBusy => Some(Tier::Input),
        ErrorTag::DaemonPreempted => Some(Tier::Input),
        ErrorTag::InternalError => Some(Tier::Tier0),
        // FIXME(JakobDegen): Make this bad experience once that's available. Usually when this
        // happens, it's probably because the user tried to shut down with Ctrl+C and something
        // about that didn't work
        ErrorTag::InterruptedByDaemonShutdown => Some(Tier::Input),
        ErrorTag::GrpcResponseMessageTooLarge => Some(Tier::Tier0),
        ErrorTag::ClientGrpc => Some(Tier::Tier0),
        ErrorTag::NoValidCerts => Some(Tier::Input),
        ErrorTag::IoBrokenPipe => None,
        ErrorTag::IoConnectionAborted => Some(Tier::Tier0),
        ErrorTag::IoNotConnected => Some(Tier::Input), // This typically means eden is not mounted
        ErrorTag::IoExecutableFileBusy => Some(Tier::Input),
        ErrorTag::IoStorageFull => Some(Tier::Input),
        ErrorTag::IoTimeout => Some(Tier::Tier0),
        ErrorTag::IoPermissionDenied => Some(Tier::Input),
        ErrorTag::IoNotFound => None,
        ErrorTag::IoSource => None,
        ErrorTag::IoSystem => None,
        ErrorTag::IoEden => None,
        ErrorTag::IoEdenConnectionError => None,
        ErrorTag::IoEdenRequestError => None,
        ErrorTag::IoEdenMountDoesNotExist => Some(Tier::Input),
        ErrorTag::IoEdenMountNotReady => Some(Tier::Tier0),
        // TODO(minglunli): Check how often Win32 Errors are actually hit, potentially do the same as POSIX
        ErrorTag::IoEdenWin32Error => Some(Tier::Tier0),
        ErrorTag::IoEdenHresultError => Some(Tier::Tier0),
        ErrorTag::IoEdenArgumentError => Some(Tier::Tier0),
        ErrorTag::IoEdenGenericError => Some(Tier::Tier0),
        ErrorTag::IoEdenMountGenerationChanged => Some(Tier::Tier0),
        ErrorTag::IoEdenJournalTruncated => Some(Tier::Tier0),
        ErrorTag::IoEdenCheckoutInProgress => Some(Tier::Input), // User switching branches during Eden operation
        ErrorTag::IoEdenOutOfDateParent => Some(Tier::Tier0),
        ErrorTag::IoEdenUnknownField => None,
        ErrorTag::IoClientBrokenPipe => Some(Tier::Environment),
        ErrorTag::ProjectMissingPath => Some(Tier::Input),
        ErrorTag::StarlarkFail => Some(Tier::Input),
        ErrorTag::StarlarkStackOverflow => Some(Tier::Input),
        ErrorTag::Visibility => Some(Tier::Input),
        ErrorTag::Analysis => Some(Tier::Input),
        ErrorTag::WatchmanTimeout => Some(Tier::Tier0),
        ErrorTag::HttpServer => Some(Tier::Tier0),
        ErrorTag::HttpClient => Some(Tier::Input),
        ErrorTag::Http => None,
        ErrorTag::AnyActionExecution => None,
        ErrorTag::AnyStarlarkEvaluation => None,
        ErrorTag::ServerStderrUnknown => None,
        ErrorTag::ServerStderrEmpty => None,
        ErrorTag::UnusedDefaultTag => None,
        ErrorTag::Install => None,
    }
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
}
