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

use bincode::Options;
use buck2_cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::BuckErrorContext;
use dice::Dice;
use dupe::Dupe;
use flate2::write::GzEncoder;
use flate2::Compression;

pub(crate) async fn dice_dump_spawn(
    dice: &Arc<Dice>,
    path: &Path,
    format: DiceDumpFormat,
) -> buck2_error::Result<()> {
    let dice = dice.dupe();
    let path = path.to_path_buf();
    tokio::task::spawn_blocking(move || dice_dump(&dice, &path, format))
        .await
        .buck_error_context("Failed to spawn")?
        .buck_error_context("Failed to dump")?;
    Ok(())
}

pub(crate) fn dice_dump(
    dice: &Arc<Dice>,
    path: &Path,
    format: DiceDumpFormat,
) -> buck2_error::Result<()> {
    match format {
        DiceDumpFormat::Tsv => dice_dump_tsv(dice, path),
        DiceDumpFormat::Bincode => dice_dump_bincode(dice, path),
        DiceDumpFormat::JsonPretty => dice_dump_json_pretty(dice, path),
    }
}

pub(crate) fn tar_dice_dump(dice_dump_folder: &Path) -> buck2_error::Result<()> {
    let tar_gz = File::create(format!("{}.tar.gz", dice_dump_folder.display()))?;
    let enc = GzEncoder::new(tar_gz, Compression::default());
    let mut tar = tar::Builder::new(enc);
    let files = vec!["nodes.gz", "edges.gz", "nodes_currently_running.gz"];
    for file_name in files {
        let mut file = File::open(dice_dump_folder.join(file_name)).buck_error_context(format!(
            "Failed to open file `{}` for compressing",
            file_name
        ))?;
        tar.append_file(file_name, &mut file)
            .buck_error_context(format!("Failed to write file `{}` to archive", file_name))?;
    }

    tar.finish()
        .buck_error_context("Failed to generate DICE dump archive")?;

    Ok(())
}

fn dice_dump_tsv(dice: &Arc<Dice>, path: &Path) -> buck2_error::Result<()> {
    let path = path.to_path_buf();
    let nodes_path = path.join("nodes.gz");
    let edges_path = path.join("edges.gz");
    let nodes_currently_running_path = path.join("nodes_currently_running.gz");

    std::fs::create_dir_all(path).buck_error_context("Failed to create directory")?;

    let nodes = File::create(&nodes_path).buck_error_context(format!(
        "Failed to open DICE node dumpfile {:?}",
        &nodes_path
    ))?;
    let mut nodes = GzEncoder::new(BufWriter::new(nodes), Compression::default());

    let edges = File::create(&edges_path).buck_error_context(format!(
        "Failed to open DICE edge dumpfile {:?}",
        &edges_path
    ))?;
    let mut edges = GzEncoder::new(BufWriter::new(edges), Compression::default());

    let nodes_currently_running =
        File::create(&nodes_currently_running_path).buck_error_context(format!(
            "Failed to open DICE node currently running dumpfile {:?}",
            &nodes_currently_running_path
        ))?;
    let mut nodes_currently_running = GzEncoder::new(
        BufWriter::new(nodes_currently_running),
        Compression::default(),
    );

    dice.serialize_tsv(&mut nodes, &mut edges, &mut nodes_currently_running)
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
        .buck_error_context("Failed to serialize")?;

    nodes
        .try_finish()
        .buck_error_context(format!("Failed to flush DICE nodes to {:?}", &nodes_path))?;
    edges
        .try_finish()
        .buck_error_context(format!("Failed to flush DICE edges to {:?}", &edges_path))?;
    nodes_currently_running
        .try_finish()
        .buck_error_context(format!(
            "Failed to flush DICE nodes currently running to {:?}",
            &nodes_currently_running_path
        ))?;

    Ok(())
}

fn dice_dump_bincode(dice: &Arc<Dice>, path: &Path) -> buck2_error::Result<()> {
    let path = path.to_path_buf();
    std::fs::create_dir_all(path.parent().unwrap())
        .buck_error_context("Failed to create directory")?;
    let out = File::create(&path)
        .buck_error_context(format!("Failed to open serde DICE dumpfile {:?}", &path))?;
    let out = GzEncoder::new(BufWriter::new(out), Compression::default());

    let mut writer = bincode::Serializer::new(
        out,
        bincode::config::DefaultOptions::new()
            .with_fixint_encoding()
            .allow_trailing_bytes(),
    );
    dice.serialize_serde(&mut writer)
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
    Ok(())
}

fn dice_dump_json_pretty(dice: &Arc<Dice>, path: &Path) -> buck2_error::Result<()> {
    let path = path.to_path_buf();
    std::fs::create_dir_all(path.parent().unwrap())
        .buck_error_context("Failed to create directory")?;
    let out = File::create(&path)
        .buck_error_context(format!("Failed to open serde DICE dumpfile {:?}", &path))?;
    let out = GzEncoder::new(BufWriter::new(out), Compression::default());

    let mut writer = serde_json::Serializer::pretty(out);
    dice.serialize_serde(&mut writer)?;
    Ok(())
}
