/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use dice::cycles::DetectCycles;
use dice::Dice;
use dice::DiceTransaction;
use dice_examples::supply_chain::Company;
use dice_examples::supply_chain::Cost;
use dice_examples::supply_chain::Resource;
use dice_examples::supply_chain::Setup;

async fn setup(companies: Vec<Company>) -> anyhow::Result<DiceTransaction> {
    let dice = Dice::builder().build(DetectCycles::Enabled);
    let ctx = dice.ctx();
    let ctx = ctx.init_state()?;

    ctx.add_companies(companies).await
}

#[tokio::test]
async fn test_no_resources() -> Result<(), Arc<anyhow::Error>> {
    let ctx = setup(vec![Company {
        name: Arc::new("hello world".to_owned()),
        makes: HashMap::new(),
    }])
    .await?;

    assert_eq!(None, ctx.resource_cost(&Resource::Wood).await?);
    let success = ctx
        .change_company_resource_cost("hello world", &Resource::Stick, 5)
        .await;
    assert!(success.is_err());
    Ok(())
}

#[tokio::test]
async fn test_other_resource() -> Result<(), Arc<anyhow::Error>> {
    let ctx = setup(vec![Company {
        name: Arc::new("hello world".to_owned()),
        makes: [(Resource::Wood, 2)].iter().cloned().collect(),
    }])
    .await?;

    assert_eq!(None, ctx.resource_cost(&Resource::Stick).await?);
    assert_eq!(Some(2), ctx.resource_cost(&Resource::Wood).await?);
    Ok(())
}

#[tokio::test]
async fn test_simple() -> Result<(), Arc<anyhow::Error>> {
    let ctx = setup(vec![
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
    ])
    .await?;

    let expected = [
        (Resource::Wood, 1),
        (Resource::Cobblestone, 7),
        (Resource::Plank, 1 + 2),
        (Resource::Stick, 2 * (1 + 2) + 4),
        (Resource::Pickaxe, 3 * 7 + 2 * (2 * (1 + 2) + 4) + 10),
        (Resource::CraftingTable, 4 * (1 + 2) + 5),
    ];

    for (resource, cost) in &expected {
        assert_eq!(
            Some(*cost),
            ctx.resource_cost(resource).await?,
            "Testing {}",
            resource
        );
    }
    Ok(())
}

#[tokio::test]
async fn test_complex() -> Result<(), Arc<anyhow::Error>> {
    let ctx = setup(vec![
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
    .await?;

    let expected = [
        (Resource::Wood, 1),
        (Resource::Cobblestone, 7),
        (Resource::Plank, 1 + 1),
        (Resource::Stick, 2 * (1 + 1) + 1),
        (Resource::Pickaxe, 3 * 7 + 2 * (2 * (1 + 1) + 1) + 5),
        (Resource::CraftingTable, 4 * (1 + 1) + 5),
    ];

    for (resource, cost) in &expected {
        assert_eq!(
            Some(*cost),
            ctx.resource_cost(resource).await?,
            "Testing {}",
            resource
        );
    }
    Ok(())
}

#[tokio::test]
async fn test_change_cost() -> Result<(), Arc<anyhow::Error>> {
    let ctx = setup(vec![
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
    ])
    .await?;

    ctx.change_company_resource_cost("Steve", &Resource::Stick, 2)
        .await
        .map_err(|e| Arc::new(anyhow::anyhow!(e)))?;

    let ctx = ctx.commit();

    assert_eq!(
        Some(3 * 7 + 2 * (2 * (1 + 2) + 2) + 10),
        ctx.resource_cost(&Resource::Pickaxe).await?
    );

    let update_success = ctx
        .change_company_resource_cost("Steve", &Resource::Pickaxe, 5)
        .await;

    assert!(update_success.is_err());

    Ok(())
}
