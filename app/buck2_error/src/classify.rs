/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_data::error::ErrorTag;

use crate::Category;

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
pub(crate) fn error_tag_category(tag: ErrorTag) -> Option<Category> {
    match tag {
        ErrorTag::ServerJemallocAssert => Some(Category::Infra),
        ErrorTag::ServerStackOverflow => Some(Category::Infra),
        ErrorTag::ServerPanicked => Some(Category::Infra),
        ErrorTag::ServerSegv => Some(Category::Infra),
        ErrorTag::DaemonWontDieFromKill => Some(Category::Infra),
        ErrorTag::DaemonConnect => None,
        ErrorTag::DaemonIsBusy => Some(Category::User),
        ErrorTag::InternalError => Some(Category::Infra),
        // FIXME(JakobDegen): Make this bad experience once that's available. Usually when this
        // happens, it's probably because the user tried to shut down with Ctrl+C and something
        // about that didn't work
        ErrorTag::InterruptedByDaemonShutdown => Some(Category::User),
        ErrorTag::GrpcResponseMessageTooLarge => Some(Category::Infra),
        ErrorTag::ClientGrpc => Some(Category::Infra),
        ErrorTag::IoBrokenPipe => None,
        ErrorTag::IoConnectionAborted => Some(Category::Infra),
        ErrorTag::IoNotConnected => Some(Category::User), // This typically means eden is not mounted
        ErrorTag::IoExecutableFileBusy => Some(Category::User),
        ErrorTag::IoStorageFull => Some(Category::User),
        ErrorTag::IoTimeout => Some(Category::Infra),
        ErrorTag::IoPermissionDenied => Some(Category::User),
        ErrorTag::IoNotFound => None,
        ErrorTag::IoSource => None,
        ErrorTag::IoSystem => None,
        ErrorTag::ProjectMissingPath => Some(Category::User),
        ErrorTag::StarlarkFail => Some(Category::User),
        ErrorTag::StarlarkStackOverflow => Some(Category::User),
        ErrorTag::Visibility => Some(Category::User),
        ErrorTag::Analysis => Some(Category::User),
        ErrorTag::WatchmanTimeout => Some(Category::Infra),
        ErrorTag::HttpServer => Some(Category::Infra),
        ErrorTag::HttpClient => Some(Category::User),
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
