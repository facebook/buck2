/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use dice::DiceComputations;
use dice::UserComputationData;
use futures::Future;
use futures::future::BoxFuture;

pub struct KeepGoing;

impl KeepGoing {
    pub fn try_compute_join_all<'a, 'd, T: Send, R: 'a, E: 'a>(
        ctx: &'a mut DiceComputations<'d>,
        items: impl IntoIterator<Item = T, IntoIter: ExactSizeIterator>,
        mapper: impl FnOnce(&'a mut DiceComputations<'d>, T) -> BoxFuture<'a, Result<R, E>>
        + Send
        + Sync
        + Copy,
    ) -> impl Future<Output = Result<Vec<R>, E>> {
        let keep_going = ctx.per_transaction_data().get_keep_going();

        let futs = ctx.compute_many(
            items
                .into_iter()
                .map(move |v| move |ctx: &'a mut DiceComputations<'d>| mapper(ctx, v)),
        );

        async move {
            Ok(if keep_going {
                futures::future::join_all(futs)
                    .await
                    .into_iter()
                    .try_collect::<Vec<_>>()?
            } else {
                buck2_util::future::try_join_all(futs).await?
            })
        }
    }
}

pub struct KeepGoingHolder(bool);

pub trait HasKeepGoing {
    fn set_keep_going(&mut self, keep_going: bool);

    fn get_keep_going(&self) -> bool;
}

impl HasKeepGoing for UserComputationData {
    fn set_keep_going(&mut self, keep_going: bool) {
        self.data.set(KeepGoingHolder(keep_going));
    }

    fn get_keep_going(&self) -> bool {
        self.data
            .get::<KeepGoingHolder>()
            .expect("KeepGoing should be set")
            .0
    }
}
