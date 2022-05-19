/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{fs::File, io::BufWriter, path::Path, sync::Arc};

use anyhow::Context as _;
use cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use dice::Dice;
use flate2::{write::GzEncoder, Compression};
use gazebo::dupe::Dupe;

use crate::daemon::server::BaseCommandContext;

pub async fn dice_dump_spawn(
    server_ctx: BaseCommandContext,
    path: &Path,
    format: DiceDumpFormat,
) -> anyhow::Result<()> {
    let dice = server_ctx.dice().dupe();
    let path = path.to_path_buf();
    tokio::task::spawn_blocking(move || dice_dump(&dice, &path, format))
        .await
        .context("Failed to spawn")?
        .context("Failed to dump")?;
    Ok(())
}

pub fn dice_dump(dice: &Arc<Dice>, path: &Path, format: DiceDumpFormat) -> anyhow::Result<()> {
    match format {
        DiceDumpFormat::Tsv => dice_dump_tsv(dice, path),
        DiceDumpFormat::Serde => dice_dump_serde(dice, path),
    }
}

pub fn dice_dump_tsv(dice: &Arc<Dice>, path: &Path) -> anyhow::Result<()> {
    let path = path.to_path_buf();
    let nodes_path = path.join("nodes.gz");
    let edges_path = path.join("edges.gz");

    std::fs::create_dir_all(path).context("Failed to create directory")?;

    let nodes = File::create(&nodes_path).context(format!(
        "Failed to open DICE node dumpfile {:?}",
        &nodes_path
    ))?;
    let mut nodes = GzEncoder::new(BufWriter::new(nodes), Compression::default());

    let edges = File::create(&edges_path).context(format!(
        "Failed to open DICE edge dumpfile {:?}",
        &edges_path
    ))?;
    let mut edges = GzEncoder::new(BufWriter::new(edges), Compression::default());

    dice.serialize_tsv(&mut nodes, &mut edges)
        .context("Failed to serialize")?;

    nodes
        .try_finish()
        .context(format!("Failed to flush DICE nodes to {:?}", &nodes_path))?;
    edges
        .try_finish()
        .context(format!("Failed to flush DICE edges to {:?}", &edges_path))?;

    Ok(())
}

pub fn dice_dump_serde(dice: &Arc<Dice>, path: &Path) -> anyhow::Result<()> {
    let path = path.to_path_buf();
    std::fs::create_dir_all(path.parent().unwrap()).context("Failed to create directory")?;
    let out =
        File::create(&path).context(format!("Failed to open serde DICE dumpfile {:?}", &path))?;
    let out = GzEncoder::new(BufWriter::new(out), Compression::default());
    dice.serialize_serde(out)?;
    Ok(())
}
