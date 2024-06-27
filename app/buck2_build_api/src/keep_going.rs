/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use dice::DiceComputations;
use dice::UserComputationData;
use futures::future::BoxFuture;
use futures::Future;

pub struct KeepGoing;

impl KeepGoing {
    pub fn try_compute_join_all<'a, T: Send, R: 'a, E: 'a>(
        ctx: &'a mut DiceComputations<'_>,
        items: impl IntoIterator<Item = T>,
        mapper: (
            impl for<'x> FnOnce(&'x mut DiceComputations<'a>, T) -> BoxFuture<'x, Result<R, E>>
            + Send
            + Sync
            + Copy
        ),
    ) -> impl Future<Output = Result<Vec<R>, E>> + 'a {
        let keep_going = ctx.per_transaction_data().get_keep_going();

        let futs = ctx.compute_many(items.into_iter().map(move |v| {
            DiceComputations::declare_closure(
                move |ctx: &mut DiceComputations| -> BoxFuture<Result<R, E>> { mapper(ctx, v) },
            )
        }));

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
