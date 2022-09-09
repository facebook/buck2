/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::time::Instant;

use async_trait::async_trait;
use buck2_data::SpanStartEvent;
use buck2_events::subscriber::EventSubscriber;
use buck2_events::BuckEvent;
use buck2_events::SpanId;
use derive_more::From;
use gazebo::prelude::*;
use linked_hash_map::LinkedHashMap;

use crate::what_ran::WhatRanRelevantAction;
use crate::what_ran::WhatRanState;

#[derive(Debug, thiserror::Error)]
enum SpanTrackerError {
    #[error("Tried to end an unstarted event: `{0:#?}`.\nStarted events: `{1:?}`.")]
    InvalidRemoval(BuckEvent, Vec<BuckEvent>),
    #[error(
        "Tried to register with a parent span that had not started: `{0:#?}`.\nStarted events: `{1:?}`."
    )]
    InvalidParent(BuckEvent, Vec<BuckEvent>),
    #[error("Tried to start an event not associated with a span: `{0:?}.")]
    NonSpanEvent(BuckEvent),
}

#[derive(Clone)]
pub struct SpanInfo {
    pub event: BuckEvent,
    pub start: Instant,
}

struct Span {
    info: SpanInfo,
    children: LinkedHashMap<SpanId, ()>,
}

pub struct SpanHandle<'a> {
    tracker: &'a SpanTracker,
    span: &'a Span,
}

impl<'a> SpanHandle<'a> {
    pub fn info(&self) -> &SpanInfo {
        &self.span.info
    }

    /// Returns how many children we expect to yield. If we received invalid data (a child was
    /// removed and didn't provide a parent ID when it did), this could be incorrect and there
    /// might be fewer (this should be infrequent)
    pub fn children_count(&self) -> usize {
        self.span.children.len()
    }

    pub fn children<'b>(&'b self) -> impl Iterator<Item = SpanHandle<'b>> + 'b
    where
        'a: 'b,
    {
        self.span.children.iter().filter_map(move |c| {
            let span = self.tracker.all.get(c.0)?;

            Some(SpanHandle {
                span,
                tracker: self.tracker,
            })
        })
    }
}

/// SpanTracker tracks ongoing spans received via handle() (those are typically produced by
/// the Buck daemon). Internally, we keep track of:
///
/// - Ongoing spans that are roots. Those will be rendered on their own line in the console.
/// - All ongoing spans by id. This is used to access spans by id, such as when looking for
///   a parent span.
///
/// Internally, Spans also reference their children.
///
/// We also keep track of how many roots have ended.
pub struct SpanTracker {
    roots: LinkedHashMap<SpanId, ()>,
    all: LinkedHashMap<SpanId, Span>,
    roots_completed: usize,
}

impl SpanTracker {
    pub fn new() -> Self {
        Self {
            roots: Default::default(),
            all: Default::default(),
            roots_completed: 0,
        }
    }

    /// Used for rendering errors.
    fn debug_known_events(&self) -> Vec<BuckEvent> {
        self.all
            .values()
            .map(|span| span.info.event.clone())
            .collect()
    }

    pub fn start_at(
        &mut self,
        start: &SpanStartEvent,
        event: &BuckEvent,
        at: Instant,
    ) -> anyhow::Result<()> {
        let is_root = span_is_root(start);
        let span_id = event
            .span_id
            .ok_or_else(|| SpanTrackerError::NonSpanEvent(event.clone()))?;

        self.all.entry(span_id).or_insert_with(|| Span {
            info: SpanInfo {
                event: event.clone(),
                start: at,
            },
            children: LinkedHashMap::new(),
        });

        if is_root {
            self.roots.insert(span_id, ());
        }

        if let Some(parent_id) = event.parent_id {
            let parent = match self.all.get_mut(&parent_id) {
                Some(parent) => parent,
                None => {
                    return Err(SpanTrackerError::InvalidParent(
                        event.clone(),
                        self.debug_known_events(),
                    )
                    .into());
                }
            };

            parent.children.insert(span_id, ());
        }

        Ok(())
    }

    pub fn iter_roots<'a>(&'a self) -> impl ExactSizeIterator<Item = SpanHandle<'a>> + 'a {
        self.roots.keys().map(move |s| {
            // NOTE: This unwrap is safe because we always insert into roots after inserting into
            // `all`, and delete from `roots` before deleting from `all`.
            let span = self
                .all
                .get(s)
                .expect("Root cannot be registered and missing from all()");

            SpanHandle {
                span,
                tracker: self,
            }
        })
    }

    pub fn roots_completed(&self) -> usize {
        self.roots_completed
    }

    /// Return if span_tracker has been used.
    pub fn is_unused(&self) -> bool {
        self.roots.is_empty() && self.roots_completed == 0
    }

    pub fn roots_ongoing(&self) -> usize {
        self.roots.len()
    }
}

#[async_trait]
impl EventSubscriber for SpanTracker {
    async fn handle_event_start(
        &mut self,
        start: &SpanStartEvent,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.start_at(start, event, Instant::now())
    }

    async fn handle_event_end(
        &mut self,
        _end: &buck2_data::SpanEndEvent,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        let span_id = event
            .span_id
            .ok_or_else(|| SpanTrackerError::NonSpanEvent(event.clone()))?;

        // This event might not be eligible as a root, but we need to maintain the invariant that
        // nothing can be in `roots` if it's not in `all` so we still have to clear it. Besides, we
        // need to find out if it was indeed a root to track roots_completed.
        if self.roots.remove(&span_id).is_some() {
            self.roots_completed += 1;
        }

        let removed = self.all.remove(&span_id).is_some();
        if !removed {
            return Err(
                SpanTrackerError::InvalidRemoval(event.clone(), self.debug_known_events()).into(),
            );
        }

        if let Some(parent_id) = event.parent_id {
            let parent = match self.all.get_mut(&parent_id) {
                Some(parent) => parent,
                None => {
                    return Err(SpanTrackerError::InvalidParent(
                        event.clone(),
                        self.debug_known_events(),
                    )
                    .into());
                }
            };

            let removed = parent.children.remove(&span_id).is_some();

            if !removed {
                return Err(SpanTrackerError::InvalidRemoval(
                    event.clone(),
                    self.debug_known_events(),
                )
                .into());
            }
        }

        Ok(())
    }
}

/// Determine whether this Span should be rendered as root (i.e. show on its own line, potentially
/// including its chldren).
fn span_is_root(event: &SpanStartEvent) -> bool {
    use buck2_data::span_start_event::Data;

    match event.data.as_ref() {
        Some(
            Data::Command(..)
            | Data::AnalysisStage(..)
            | Data::ExecutorStage(..)
            | Data::MatchDepFiles(..),
        ) => false,
        Some(
            Data::ActionExecution(..)
            | Data::FinalMaterialization(..)
            | Data::Analysis(..)
            | Data::Load(..)
            | Data::LoadPackage(..)
            | Data::TestDiscovery(..)
            | Data::TestStart(..)
            | Data::Watchman(..)
            | Data::SharedTask(..)
            | Data::CacheUpload(..)
            | Data::Fake(..),
        ) => true,
        None => false,
    }
}

impl WhatRanState<OptionalSpanId> for SpanTracker {
    fn get(&self, span_id: OptionalSpanId) -> Option<WhatRanRelevantAction<'_>> {
        let span_id = span_id.0?;

        self.all
            .get(&span_id)
            .map(|e| &e.info.event.data)
            .and_then(WhatRanRelevantAction::from_buck_data)
    }
}

/// A wrapper type to make calls to emit_event_if_relevant more convenient, since parent_id is
/// Option<SpanId> on buck2_events::BuckEvent.
#[derive(From, Copy, Clone, Dupe)]
pub struct OptionalSpanId(Option<SpanId>);

impl fmt::Display for OptionalSpanId {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        if let Some(this) = self.0 {
            write!(formatter, "{}", this)
        } else {
            write!(formatter, "(none)")
        }
    }
}
