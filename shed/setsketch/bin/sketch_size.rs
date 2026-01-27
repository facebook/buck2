/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::io;
use std::io::BufRead;

use anyhow::Context;
use anyhow::bail;
use base64::Engine;
use setsketch::SetSketch;
use setsketch::SetSketchParams;

fn decode_sketch(sketch_str: &str) -> anyhow::Result<SetSketch> {
    let sketch_str = sketch_str.trim();

    // Decode base64
    let bytes = base64::engine::general_purpose::STANDARD_NO_PAD
        .decode(sketch_str)
        .context("Failed to decode base64")?;

    // Convert bytes to u16 registers (native endian)
    if bytes.len() % 2 != 0 {
        bail!(
            "Invalid sketch data: byte count {} must be even",
            bytes.len()
        );
    }

    let registers: Vec<u16> = bytes
        .chunks_exact(2)
        .map(|chunk| u16::from_ne_bytes([chunk[0], chunk[1]]))
        .collect();

    // Create sketch
    let params = SetSketchParams::recommended();
    Ok(SetSketch::from_registers(params, registers))
}

fn main() -> anyhow::Result<()> {
    let stdin = io::stdin();
    let lines: Vec<String> = stdin.lock().lines().collect::<Result<_, _>>()?;

    if lines.is_empty() {
        bail!("No input provided. Please provide one or more sketch strings.");
    }

    // Decode all sketches
    let mut sketches: Vec<SetSketch> = Vec::new();
    for line in &lines {
        if !line.trim().is_empty() {
            sketches.push(decode_sketch(line)?);
        }
    }

    if sketches.is_empty() {
        bail!("No valid sketches found in input");
    }

    // Merge all sketches
    let mut sketches_iter = sketches.into_iter();
    let mut merged = sketches_iter.next().unwrap();
    for sketch in sketches_iter {
        merged.merge(&sketch);
    }

    // Compute and print cardinality
    let cardinality = merged.cardinality();
    println!("{}", cardinality);

    Ok(())
}
