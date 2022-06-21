/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))]
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

mod common;

use std::sync::Arc;

use async_trait::async_trait;
use common::BenchmarkComputationsPrerequisites;
use dice::DiceTransaction;
use dice_examples::supply_chain::Company;
use dice_examples::supply_chain::Cost;
use dice_examples::supply_chain::Resource;
use dice_examples::supply_chain::Setup;
use futures::stream::StreamExt;
use futures::stream::{self};

struct SupplyChainBenchmark;

#[async_trait]
impl BenchmarkComputationsPrerequisites for SupplyChainBenchmark {
    type Key = Resource;
    type Updater = (&'static str, Resource, u16);
    type Value = Option<u16>;

    async fn fresh(ctx: DiceTransaction) -> DiceTransaction {
        let ctx = ctx.init_state();
        ctx.add_companies(vec![
            Company {
                name: Arc::new("Steve".to_owned()),
                makes: [
                    (Resource::Plank, 2),
                    (Resource::Wood, 1),
                    (Resource::Stick, 4),
                    (Resource::Cobblestone, 7),
                ]
                .iter()
                .cloned()
                .collect(),
            },
            Company {
                name: Arc::new("Alex".to_owned()),
                makes: [(Resource::Pickaxe, 10), (Resource::CraftingTable, 5)]
                    .iter()
                    .cloned()
                    .collect(),
            },
            Company {
                name: Arc::new("Bob".to_owned()),
                makes: [
                    (Resource::Pickaxe, 5),
                    (Resource::Plank, 1),
                    (Resource::Stick, 2),
                ]
                .iter()
                .cloned()
                .collect(),
            },
            Company {
                name: Arc::new("Raj".to_owned()),
                makes: [(Resource::Stick, 1)].iter().cloned().collect(),
            },
        ])
        .await
    }

    async fn update<I>(ctx: DiceTransaction, keys: I) -> DiceTransaction
    where
        I::IntoIter: Send,
        I: IntoIterator<Item = Self::Updater> + Send + Sync,
    {
        stream::iter(keys)
            .for_each(|(company, resource, cost)| {
                let ctx = &ctx;
                async move {
                    ctx.change_company_resource_cost(company, &resource, cost)
                        .await
                        .expect("could not change cost");
                }
            })
            .await;

        ctx.commit()
    }

    async fn compute(ctx: &DiceTransaction, key: Self::Key) -> Self::Value {
        ctx.resource_cost(&key).await
    }

    fn invalidated_recompute() -> (Vec<Self::Updater>, Self::Key) {
        (
            vec![
                ("Steve", Resource::Cobblestone, 4),
                ("Raj", Resource::Stick, 10),
            ],
            Resource::Pickaxe,
        )
    }
    fn early_cutoff_recompute() -> (Vec<Self::Updater>, Self::Key) {
        (
            vec![
                ("Steve", Resource::Cobblestone, 7),
                ("Raj", Resource::Stick, 1),
            ],
            Resource::Pickaxe,
        )
    }

    fn get_sample_updates() -> Vec<Self::Updater> {
        vec![
            ("Steve", Resource::Cobblestone, 7),
            ("Raj", Resource::Stick, 1),
        ]
    }

    fn get_sample_key() -> Self::Key {
        Resource::Pickaxe
    }
}

benchmark!(supply_chain_benchmark);
