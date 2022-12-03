/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Evaluates a series of futures, continuing past the first error if appropriate.
//! Use the environment variable BUCK2_KEEP_GOING to control this behaviour.

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Display;
use std::fs::File;
use std::hash::Hash;
use std::io::Write;
use std::sync::Mutex;

use futures::Future;
use futures::Stream;
use futures::StreamExt;
use indexmap::IndexMap;
use once_cell::sync::Lazy;
use smallvec::SmallVec;

/// Should we keep going after receiving the first error, or immediately terminate.
/// FIXME: This should be a runtime flag rather than driven by an environment variable.
static KEEP_GOING: Lazy<Option<KeepGoing>> = Lazy::new(|| {
    let name = std::env::var("BUCK2_KEEP_GOING").ok()?;
    Some(KeepGoing::new(&name))
});

struct KeepGoing(Mutex<KeepGoingData>);

struct KeepGoingData {
    /// The file to write keep_going information to
    file: File,
    /// The error messages we've already reported. We use the hash of the message to dedupe.
    /// Should be per-session rather than global.
    seen: HashSet<blake3::Hash>,
}

impl KeepGoing {
    fn new(name: &str) -> KeepGoing {
        let file = File::create(name).unwrap_or_else(|e| {
            panic!(
                "Failed to open $BUCK2_KEEP_GOING file, file `{}`, error `{}`",
                name, e
            );
        });
        Self(Mutex::new(KeepGoingData {
            file,
            seen: HashSet::new(),
        }))
    }

    fn record<E: Display>(&self, e: E) {
        let msg = format!("{:#}", e);
        let hash = blake3::hash(msg.as_bytes());
        {
            let mut x = self.0.lock().unwrap();
            if x.seen.insert(hash) {
                // This hash was not in the set, so print it out
                writeln!(
                    x.file,
                    "{:-<70}\nError (produced due to keep-going):\n{}\n{:-<70}",
                    "", msg, ""
                )
                .expect("Failed to write to $BUCK2_KEEP_GOING file");
            }
        }
    }

    async fn process<R, E: Display>(&self, mut x: impl Stream<Item = Result<R, E>> + Unpin) {
        // TODO: We should spawn these futures into another queue and return immediately.
        //       Once we do that, this function won't be async.

        while let Some(x) = x.next().await {
            if let Err(e) = x {
                self.record(e)
            }
        }
    }
}

/// Evaluate a series of futures, returning a series of results.
/// If any future fails, it will fail.
/// If KEEP_GOING is true, it will first make all others continue.
pub async fn try_join_all<C, R, E: Display>(
    mut inputs: impl Stream<Item = Result<R, E>> + Unpin,
) -> Result<C, E>
where
    C: KeepGoingCollectable<R>,
{
    let size = inputs.size_hint().0;
    let mut res = C::with_capacity(size);
    while let Some(x) = inputs.next().await {
        match x {
            Ok(x) => res.push(x),
            Err(e) => {
                // One a build fails we call `keep_going` on the remainder of the operations
                // and then fail (ideally before waiting for them all to finish).
                if let Some(keep_going) = Lazy::force(&KEEP_GOING) {
                    keep_going.record(&e);
                    keep_going.process(inputs).await;
                }
                return Err(e);
            }
        }
    }
    Ok(res)
}

/// Similar to try_join_all, but this is meant to combine the outcomes of try_join_all, so it
/// doesn't do any recording on its own.
pub async fn try_join<A, B, E>(
    a: impl Future<Output = Result<A, E>>,
    b: impl Future<Output = Result<B, E>>,
) -> Result<(A, B), E> {
    if Lazy::force(&KEEP_GOING).is_some() {
        let (a, b) = futures::future::join(a, b).await;
        Ok((a?, b?))
    } else {
        futures::future::try_join(a, b).await
    }
}

pub trait KeepGoingCollectable<I> {
    fn with_capacity(cap: usize) -> Self;

    fn push(&mut self, item: I);
}

impl<K, V> KeepGoingCollectable<(K, V)> for IndexMap<K, V>
where
    K: PartialEq + Eq + Hash,
{
    fn with_capacity(cap: usize) -> Self {
        IndexMap::with_capacity(cap)
    }

    fn push(&mut self, item: (K, V)) {
        let (k, v) = item;
        IndexMap::insert(self, k, v);
    }
}

impl<K, V> KeepGoingCollectable<(K, V)> for HashMap<K, V>
where
    K: PartialEq + Eq + Hash,
{
    fn with_capacity(cap: usize) -> Self {
        HashMap::with_capacity(cap)
    }

    fn push(&mut self, item: (K, V)) {
        let (k, v) = item;
        HashMap::insert(self, k, v);
    }
}

impl<I> KeepGoingCollectable<I> for Vec<I> {
    fn with_capacity(cap: usize) -> Self {
        Vec::with_capacity(cap)
    }

    fn push(&mut self, item: I) {
        Vec::push(self, item);
    }
}

impl<I> KeepGoingCollectable<I> for SmallVec<[I; 1]> {
    fn with_capacity(cap: usize) -> Self {
        SmallVec::with_capacity(cap)
    }

    fn push(&mut self, item: I) {
        SmallVec::push(self, item);
    }
}
