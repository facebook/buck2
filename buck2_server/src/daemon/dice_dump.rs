/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs::File;
use std::io::BufWriter;
use std::path::Path;
use std::sync::Arc;

use anyhow::Context as _;
use bincode::Options;
use cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use dice::Dice;
use flate2::write::GzEncoder;
use flate2::Compression;
use gazebo::dupe::Dupe;

pub async fn dice_dump_spawn(
    dice: &Arc<Dice>,
    path: &Path,
    format: DiceDumpFormat,
) -> anyhow::Result<()> {
    let dice = dice.dupe();
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
        DiceDumpFormat::SerdePretty => dice_dump_serde_pretty(dice, path),
    }
}

pub fn dice_dump_tsv(dice: &Arc<Dice>, path: &Path) -> anyhow::Result<()> {
    let path = path.to_path_buf();
    let nodes_path = path.join("nodes.gz");
    let edges_path = path.join("edges.gz");
    let nodes_currently_running_path = path.join("nodes_currently_running.gz");

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

    let nodes_currently_running = File::create(&nodes_currently_running_path).context(format!(
        "Failed to open DICE node currently running dumpfile {:?}",
        &nodes_currently_running_path
    ))?;
    let mut nodes_currently_running = GzEncoder::new(
        BufWriter::new(nodes_currently_running),
        Compression::default(),
    );

    dice.serialize_tsv(&mut nodes, &mut edges, &mut nodes_currently_running)
        .context("Failed to serialize")?;

    nodes
        .try_finish()
        .context(format!("Failed to flush DICE nodes to {:?}", &nodes_path))?;
    edges
        .try_finish()
        .context(format!("Failed to flush DICE edges to {:?}", &edges_path))?;
    nodes_currently_running.try_finish().context(format!(
        "Failed to flush DICE nodes currently running to {:?}",
        &nodes_currently_running_path
    ))?;

    Ok(())
}

pub fn dice_dump_serde(dice: &Arc<Dice>, path: &Path) -> anyhow::Result<()> {
    let path = path.to_path_buf();
    std::fs::create_dir_all(path.parent().unwrap()).context("Failed to create directory")?;
    let out =
        File::create(&path).context(format!("Failed to open serde DICE dumpfile {:?}", &path))?;
    let out = GzEncoder::new(BufWriter::new(out), Compression::default());

    let mut writer = bincode::Serializer::new(
        out,
        bincode::config::DefaultOptions::new()
            .with_fixint_encoding()
            .allow_trailing_bytes(),
    );
    dice.serialize_serde(&mut writer)?;
    Ok(())
}

pub fn dice_dump_serde_pretty(dice: &Arc<Dice>, path: &Path) -> anyhow::Result<()> {
    let path = path.to_path_buf();
    std::fs::create_dir_all(path.parent().unwrap()).context("Failed to create directory")?;
    let out =
        File::create(&path).context(format!("Failed to open serde DICE dumpfile {:?}", &path))?;
    let out = GzEncoder::new(BufWriter::new(out), Compression::default());

    let mut writer = serde_json::Serializer::pretty(out);
    dice.serialize_serde(&mut writer)?;
    Ok(())
}
