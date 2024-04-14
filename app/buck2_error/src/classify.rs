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
        ErrorTag::DaemonConnect => line!(),
        ErrorTag::GrpcResponseMessageTooLarge => line!(),
        ErrorTag::ClientGrpc => line!(),
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
        ErrorTag::Analysis => line!(),
        ErrorTag::AnyActionExecution => line!(),
        ErrorTag::AnyStarlarkEvaluation => line!(),
        ErrorTag::UnusedDefaultTag => line!(),
    }
}

/// Some tags are known to be either infrastructure or user errors.
pub(crate) fn error_tag_category(tag: ErrorTag) -> Option<Tier> {
    match tag {
        ErrorTag::ServerJemallocAssert => Some(Tier::Infra),
        ErrorTag::ServerStackOverflow => Some(Tier::Infra),
        ErrorTag::ServerPanicked => Some(Tier::Infra),
        ErrorTag::ServerSegv => Some(Tier::Infra),
        ErrorTag::DaemonWontDieFromKill => Some(Tier::Infra),
        ErrorTag::DaemonConnect => None,
        ErrorTag::DaemonIsBusy => Some(Tier::User),
        ErrorTag::InternalError => Some(Tier::Infra),
        // FIXME(JakobDegen): Make this bad experience once that's available. Usually when this
        // happens, it's probably because the user tried to shut down with Ctrl+C and something
        // about that didn't work
        ErrorTag::InterruptedByDaemonShutdown => Some(Tier::User),
        ErrorTag::GrpcResponseMessageTooLarge => Some(Tier::Infra),
        ErrorTag::ClientGrpc => Some(Tier::Infra),
        ErrorTag::IoBrokenPipe => None,
        ErrorTag::IoConnectionAborted => Some(Tier::Infra),
        ErrorTag::IoNotConnected => Some(Tier::User), // This typically means eden is not mounted
        ErrorTag::IoExecutableFileBusy => Some(Tier::User),
        ErrorTag::IoStorageFull => Some(Tier::User),
        ErrorTag::IoTimeout => Some(Tier::Infra),
        ErrorTag::IoPermissionDenied => Some(Tier::User),
        ErrorTag::IoNotFound => None,
        ErrorTag::IoSource => None,
        ErrorTag::IoSystem => None,
        ErrorTag::ProjectMissingPath => Some(Tier::User),
        ErrorTag::StarlarkFail => Some(Tier::User),
        ErrorTag::StarlarkStackOverflow => Some(Tier::User),
        ErrorTag::Visibility => Some(Tier::User),
        ErrorTag::Analysis => Some(Tier::User),
        ErrorTag::WatchmanTimeout => Some(Tier::Infra),
        ErrorTag::HttpServer => Some(Tier::Infra),
        ErrorTag::HttpClient => Some(Tier::User),
        ErrorTag::Http => None,
        ErrorTag::AnyActionExecution => None,
        ErrorTag::AnyStarlarkEvaluation => None,
        ErrorTag::ServerStderrUnknown => None,
        ErrorTag::ServerStderrEmpty => None,
        ErrorTag::UnusedDefaultTag => None,
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
