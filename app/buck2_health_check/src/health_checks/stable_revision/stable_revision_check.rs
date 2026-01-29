/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![allow(dead_code)]
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use itertools::Itertools;

use crate::health_checks::stable_revision::bookmark_revision_fetcher::BookmarkRevisionFetcher;
use crate::health_checks::stable_revision::stable_revision_fetcher::StableRevisionFetcher;
use crate::health_checks::stable_revision::target_bookmark_map::TargetBookmarkMap;
use crate::interface::HealthCheck;
use crate::interface::HealthCheckContext;
use crate::interface::HealthCheckSnapshotData;
use crate::report::DisplayReport;
use crate::report::HealthIssue;
use crate::report::Message;
use crate::report::Remediation;
use crate::report::Report;
use crate::report::Severity;

const RE_CACHE_TTL_HOURS: u64 = 36;
const REMEDIATION_LINK: &str = "https://fburl.com/buck2_stable_revision";
const STABLE_REVISION_TAG: &str = "not_on_stable_revision";
const STABLE_INFO_NOT_AVAILABLE_TAG: &str = "stable_info_not_available";

#[derive(Debug, PartialEq)]
enum BookmarksRecommendation {
    NotComputed,
    NotOnStableRevision(HashSet<String>),
    OnStableRevision,
    StableInfoNotAvailable,
}

/// Checks if the current source control revision is on the stable revision defined for the target.
pub struct StableRevisionCheck {
    bookmark_details_fetcher: Arc<dyn BookmarkRevisionFetcher>,
    target_prefix_to_bookmark_map: TargetBookmarkMap,
    /// This list is computed once when all the context data is ready.
    /// Considering the lifetime of a 'StableRevisionCheck' is tied to a command presently, this does not need to be re-computed.
    recommended_bookmarks: BookmarksRecommendation,
}

impl StableRevisionCheck {
    pub fn new() -> buck2_error::Result<Self> {
        Ok(Self::new_with_bookmark_map_and_fetcher(
            Arc::new(StableRevisionFetcher::new()),
            TargetBookmarkMap::new_with_facebook_defaults()?,
        ))
    }

    fn new_with_bookmark_map_and_fetcher(
        bookmark_details_fetcher: Arc<dyn BookmarkRevisionFetcher>,
        target_prefix_to_bookmark_map: TargetBookmarkMap,
    ) -> Self {
        Self {
            bookmark_details_fetcher,
            target_prefix_to_bookmark_map,
            recommended_bookmarks: BookmarksRecommendation::NotComputed,
        }
    }

    pub(crate) fn run(&self) -> Option<Report> {
        match &self.recommended_bookmarks {
            BookmarksRecommendation::StableInfoNotAvailable => Some(Report {
                display_report: None,
                tag: Some(STABLE_INFO_NOT_AVAILABLE_TAG.to_owned()),
            }),
            BookmarksRecommendation::NotOnStableRevision(bookmarks) => Some(Report {
                display_report: Self::generate_display_report(bookmarks),
                tag: Some(STABLE_REVISION_TAG.to_owned()),
            }),
            _ => None,
        }
    }

    async fn try_compute_recommended_bookmarks(&mut self, context: &HealthCheckContext) {
        if !matches!(
            self.recommended_bookmarks,
            BookmarksRecommendation::NotComputed
        ) || !Self::can_run(context)
        {
            return;
        }

        let target_to_bookmarks_map = self.target_bookmarks(
            context
                .parsed_target_patterns
                .as_ref()
                .expect("Target patterns not set"),
        );

        let all_bookmarks: HashSet<String> = target_to_bookmarks_map
            .values()
            .flat_map(|bookmarks| bookmarks.iter().cloned())
            .collect();

        let branched_from_revision = context
            .branched_from_revision
            .as_ref()
            .expect("Missing current revision hash");

        let bookmarks_fetcher = &self.bookmark_details_fetcher;
        let futures = all_bookmarks.iter().map(|bookmark| async move {
            // RE maintains a cache with TTL of 36 hours. Fetch only revisions marked stable in the past 36 hours.
            (
                bookmark,
                bookmarks_fetcher
                    .get_recent_revisions_for_bookmark(bookmark, RE_CACHE_TTL_HOURS)
                    .await,
            )
        });
        let recent_revisions = futures::future::join_all(futures).await;

        let mut target_to_recommendation: HashMap<String, BookmarksRecommendation> = HashMap::new();

        for (target, bookmarks) in target_to_bookmarks_map {
            let mut target_on_stable = false;
            let mut stable_info_available = false;
            // For the bookmarks of a target, check if the branched_from_revision is present in any of the recent revisions.
            for bookmark in &bookmarks {
                let revisions = recent_revisions.iter().find_map(|(b, r)| {
                    if **b == *bookmark {
                        r.as_ref()
                            .ok()
                            .and_then(|opt: &Option<Vec<String>>| opt.as_ref())
                    } else {
                        None
                    }
                });

                if let Some(revisions) = revisions {
                    stable_info_available = true;
                    if revisions.contains(branched_from_revision) {
                        target_on_stable = true;
                        break;
                    }
                }
            }
            if !stable_info_available {
                target_to_recommendation
                    .insert(target, BookmarksRecommendation::StableInfoNotAvailable);
            } else if !target_on_stable {
                if let Some(first_bookmark) = bookmarks.first() {
                    // recommended_bookmarks.insert(first_bookmark.clone());
                    target_to_recommendation.insert(
                        target,
                        BookmarksRecommendation::NotOnStableRevision(HashSet::from([
                            first_bookmark.clone(),
                        ])),
                    );
                }
            }
        }

        let mut recommended_bookmarks: HashSet<String> = HashSet::new();
        for recommendation in target_to_recommendation.values() {
            match recommendation {
                //If the stable info of any target is not available, we cannot recommend a stable revision.
                BookmarksRecommendation::StableInfoNotAvailable => {
                    self.recommended_bookmarks = BookmarksRecommendation::StableInfoNotAvailable;
                    return;
                }
                BookmarksRecommendation::NotOnStableRevision(bookmarks) => {
                    if let Some(first_bookmark) = bookmarks.iter().next() {
                        recommended_bookmarks.insert(first_bookmark.clone());
                    }
                }
                _ => {}
            }
        }

        if recommended_bookmarks.is_empty() {
            self.recommended_bookmarks = BookmarksRecommendation::OnStableRevision;
        } else {
            self.recommended_bookmarks =
                BookmarksRecommendation::NotOnStableRevision(recommended_bookmarks);
        }
    }

    fn can_run(context: &HealthCheckContext) -> bool {
        fn is_stable_revision_check_enabled(context: &HealthCheckContext) -> bool {
            context
                .experiment_configurations
                .as_ref()
                .is_some_and(|config| config.enable_stable_revision_check.unwrap_or(false))
        }

        is_stable_revision_check_enabled(context)
            && context.branched_from_revision.is_some()
            && context.parsed_target_patterns.is_some()
            && context.has_excess_cache_misses
            && context.command_data.as_ref().is_some_and(|data| {
                matches!(
                    data,
                    buck2_data::command_start::Data::Build(..)
                        | buck2_data::command_start::Data::Test(..)
                        | buck2_data::command_start::Data::Install(..)
                )
            })
    }

    fn target_bookmarks(
        &self,
        targets: &buck2_data::ParsedTargetPatterns,
    ) -> HashMap<String, Vec<String>> {
        targets
            .target_patterns
            .iter()
            .map(|target| {
                (
                    target.value.clone(),
                    self.target_prefix_to_bookmark_map
                        .get_best_bookmarks(&target.value),
                )
            })
            .collect()
    }

    fn generate_display_report(bookmarks: &HashSet<String>) -> Option<DisplayReport> {
        Some(DisplayReport {
            health_check_type: crate::interface::HealthCheckType::StableRevision,
            health_issue: Self::generate_warning(bookmarks),
        })
    }

    fn generate_warning(bookmarks: &HashSet<String>) -> Option<HealthIssue> {
        Some(HealthIssue {
            severity: Severity::Warning,
            message: Message::Simple(format!(
                "Rebase to a stable revision to ensure optimal build speed and cache utilization. Recommended bookmarks: {}",
                bookmarks.iter().join(",")
            )),
            remediation: Some(Remediation::Link(REMEDIATION_LINK.to_owned())),
        })
    }
}

#[async_trait::async_trait]
impl HealthCheck for StableRevisionCheck {
    fn run_check(
        &mut self,
        _snapshot: HealthCheckSnapshotData,
    ) -> buck2_error::Result<Option<Report>> {
        Ok(self.run())
    }

    async fn handle_context_update(&mut self, context: &HealthCheckContext) {
        self.try_compute_recommended_bookmarks(context).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockBookmarkRevisionFetcher {}

    const FBCODE_BOOKMARK: &str = "fbcode";
    const RL_BOOKMARK: &str = "rl_land";
    const STABLE_REVISION: &str = "304be95551a07cddaf39e03de82abe480a66f890";
    const STABLE_REVISION_2: &str = "e2518101869be9e5192ee52f56afdc7812d9b6a6";
    const UNSTABLE_REVISION: &str = "c9bf9e5716854c89bafbff5af830034a1b8c4b5b";

    #[async_trait::async_trait]
    impl BookmarkRevisionFetcher for MockBookmarkRevisionFetcher {
        async fn get_recent_revisions_for_bookmark(
            &self,
            bookmark: &str,
            _lookback_hours: u64,
        ) -> buck2_error::Result<Option<Vec<String>>> {
            match bookmark {
                FBCODE_BOOKMARK => Ok(Some(vec![
                    STABLE_REVISION.to_owned(),
                    STABLE_REVISION_2.to_owned(),
                ])),
                RL_BOOKMARK => Ok(Some(vec![STABLE_REVISION.to_owned()])),
                _ => Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Failed to get version for bookmark `invalid`",
                )),
            }
        }
    }

    fn test_map() -> TargetBookmarkMap {
        TargetBookmarkMap::new_with_target_prefix_to_bookmark_map(vec![
            (
                regex::Regex::new(r"^fbcode").unwrap(),
                vec![FBCODE_BOOKMARK.to_owned()],
            ),
            (
                regex::Regex::new(r"^fbsource//arvr/tools").unwrap(),
                vec![RL_BOOKMARK.to_owned()],
            ),
        ])
    }

    fn test_stable_revision_checker() -> StableRevisionCheck {
        StableRevisionCheck::new_with_bookmark_map_and_fetcher(
            Arc::new(MockBookmarkRevisionFetcher {}),
            test_map(),
        )
    }

    fn valid_health_check_context(revision: &str) -> HealthCheckContext {
        HealthCheckContext {
            branched_from_revision: Some(revision.to_owned()),
            parsed_target_patterns: Some(target_patterns("fbcode//tools".to_owned())),
            has_excess_cache_misses: true,
            command_data: Some(buck2_data::command_start::Data::Build(
                buck2_data::BuildCommandStart {
                    ..Default::default()
                },
            )),
            experiment_configurations: Some(buck2_data::SystemInfo {
                enable_stable_revision_check: Some(true),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    fn invalid_health_check_context() -> HealthCheckContext {
        HealthCheckContext {
            branched_from_revision: None,
            parsed_target_patterns: None,
            has_excess_cache_misses: false,
            command_data: None,
            ..Default::default()
        }
    }

    fn target_patterns(pattern: String) -> buck2_data::ParsedTargetPatterns {
        buck2_data::ParsedTargetPatterns {
            target_patterns: vec![buck2_data::TargetPattern {
                value: pattern,
                ..Default::default()
            }],
            ..Default::default()
        }
    }

    #[tokio::test]
    async fn test_get_invalid_bookmark() -> buck2_error::Result<()> {
        let checker = test_stable_revision_checker();
        let result = checker.target_bookmarks(&target_patterns("foo//bar".to_owned()));
        assert!(result.contains_key("foo//bar"));
        assert!(result["foo//bar"].is_empty());
        Ok(())
    }

    #[tokio::test]
    async fn test_get_valid_bookmark() -> buck2_error::Result<()> {
        let checker = test_stable_revision_checker();
        let result = checker.target_bookmarks(&target_patterns("fbcode//tools".to_owned()));
        assert!(result.contains_key("fbcode//tools"));
        assert!(result["fbcode//tools"].contains(&FBCODE_BOOKMARK.to_owned()));
        Ok(())
    }

    #[tokio::test]
    async fn test_can_run() -> buck2_error::Result<()> {
        let context = valid_health_check_context(STABLE_REVISION);
        assert!(StableRevisionCheck::can_run(&context));
        Ok(())
    }

    #[tokio::test]
    async fn test_cannot_run() -> buck2_error::Result<()> {
        let context = invalid_health_check_context();
        assert!(!StableRevisionCheck::can_run(&context));
        Ok(())
    }

    #[tokio::test]
    async fn test_run_on_stable_revision() -> buck2_error::Result<()> {
        let context = valid_health_check_context(STABLE_REVISION);
        let mut checker = test_stable_revision_checker();
        checker.try_compute_recommended_bookmarks(&context).await;
        assert_eq!(
            checker.recommended_bookmarks,
            BookmarksRecommendation::OnStableRevision
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_run_on_unstable_revision() -> buck2_error::Result<()> {
        let context = valid_health_check_context(UNSTABLE_REVISION);
        let mut checker = test_stable_revision_checker();
        checker.try_compute_recommended_bookmarks(&context).await;

        assert_eq!(
            checker.recommended_bookmarks,
            BookmarksRecommendation::NotOnStableRevision(HashSet::from([
                FBCODE_BOOKMARK.to_owned()
            ]))
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_run_on_invalid_context() -> buck2_error::Result<()> {
        let context = invalid_health_check_context();
        let mut checker = test_stable_revision_checker();
        checker.try_compute_recommended_bookmarks(&context).await;
        assert_eq!(
            checker.recommended_bookmarks,
            BookmarksRecommendation::NotComputed
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_target_computation_not_updated() -> buck2_error::Result<()> {
        // Test situation when we miss events and haven't computed targets_not_on_stable.
        assert_eq!(
            test_stable_revision_checker().recommended_bookmarks,
            BookmarksRecommendation::NotComputed
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_report_on_unstable_revision() -> buck2_error::Result<()> {
        let context = valid_health_check_context(UNSTABLE_REVISION);
        let mut checker = test_stable_revision_checker();
        checker.try_compute_recommended_bookmarks(&context).await;
        let report = checker.run();
        assert!(report.is_some());
        let report = report.unwrap();
        assert_eq!(report.tag, Some(STABLE_REVISION_TAG.to_owned()));
        let display_report = report.display_report.unwrap();
        let health_issue = display_report.health_issue.unwrap();
        let message_text = health_issue.message.to_string();
        assert!(message_text.contains("Rebase to a stable revision"));
        assert!(message_text.contains(FBCODE_BOOKMARK));
        Ok(())
    }

    #[tokio::test]
    async fn test_multiple_bookmarks() -> buck2_error::Result<()> {
        let mut context = valid_health_check_context(UNSTABLE_REVISION);
        context.parsed_target_patterns = Some(buck2_data::ParsedTargetPatterns {
            target_patterns: vec![
                buck2_data::TargetPattern {
                    value: "fbcode//tools".to_owned(),
                    ..Default::default()
                },
                buck2_data::TargetPattern {
                    value: "fbsource//arvr/tools".to_owned(),
                    ..Default::default()
                },
            ],
            ..Default::default()
        });

        let mut checker = test_stable_revision_checker();
        checker.try_compute_recommended_bookmarks(&context).await;
        let report = checker.run();
        assert!(report.is_some());
        let report = report.unwrap();
        assert_eq!(report.tag, Some(STABLE_REVISION_TAG.to_owned()));
        let display_report = report.display_report.unwrap();
        let health_issue = display_report.health_issue.unwrap();
        println!("{:?}", health_issue.message);
        let message_text = health_issue.message.to_string();
        assert!(message_text.contains("Rebase to a stable revision"));
        assert!(message_text.contains(FBCODE_BOOKMARK));
        assert!(message_text.contains(RL_BOOKMARK));
        Ok(())
    }

    #[tokio::test]
    async fn test_report_stable_info_not_available() -> buck2_error::Result<()> {
        let context = valid_health_check_context(UNSTABLE_REVISION);
        let mut checker = StableRevisionCheck::new_with_bookmark_map_and_fetcher(
            Arc::new(MockBookmarkRevisionFetcher {}),
            TargetBookmarkMap::new_with_target_prefix_to_bookmark_map(vec![(
                regex::Regex::new(r"^invalid").unwrap(),
                vec![FBCODE_BOOKMARK.to_owned()],
            )]),
        );
        checker.try_compute_recommended_bookmarks(&context).await;
        let report = checker.run();
        assert!(report.is_some());
        let report = report.unwrap();
        assert_eq!(report.tag, Some(STABLE_INFO_NOT_AVAILABLE_TAG.to_owned()));
        Ok(())
    }

    #[tokio::test]
    async fn test_report_stable_info_not_available_due_to_some_targets() -> buck2_error::Result<()>
    {
        let mut context = valid_health_check_context(STABLE_REVISION);
        context.parsed_target_patterns = Some(buck2_data::ParsedTargetPatterns {
            target_patterns: vec![
                buck2_data::TargetPattern {
                    value: "fbcode//tools".to_owned(),
                    ..Default::default()
                },
                buck2_data::TargetPattern {
                    value: "other//target".to_owned(),
                    ..Default::default()
                },
            ],
            ..Default::default()
        });
        let mut checker = StableRevisionCheck::new_with_bookmark_map_and_fetcher(
            Arc::new(MockBookmarkRevisionFetcher {}),
            TargetBookmarkMap::new_with_target_prefix_to_bookmark_map(vec![
                (
                    regex::Regex::new(r"^fbcode").unwrap(),
                    vec![FBCODE_BOOKMARK.to_owned()],
                ),
                (
                    regex::Regex::new(r"^invalid").unwrap(),
                    vec![FBCODE_BOOKMARK.to_owned()],
                ),
            ]),
        );
        checker.try_compute_recommended_bookmarks(&context).await;
        let report = checker.run();
        assert!(report.is_some());
        let report = report.unwrap();
        assert_eq!(report.tag, Some(STABLE_INFO_NOT_AVAILABLE_TAG.to_owned()));
        Ok(())
    }
}
