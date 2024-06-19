/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// If this code needs fixing, make sure you fix the README.md too!

use superconsole::components::bordering::Bordered;
use superconsole::components::bordering::BorderedSpec;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Lines;
use superconsole::SuperConsole;

#[derive(Debug)]
struct HelloWorld;

impl Component for HelloWorld {
    fn draw_unchecked(&self, _dimensions: Dimensions, _mode: DrawMode) -> anyhow::Result<Lines> {
        Ok(Lines(vec![
            vec!["Hello world!".to_owned()].try_into().unwrap(),
        ]))
    }
}

pub fn main() -> anyhow::Result<()> {
    let bordering = BorderedSpec::default();
    let mut superconsole = SuperConsole::new().ok_or_else(|| anyhow::anyhow!("Not a TTY"))?;
    let component = Bordered::new(HelloWorld, bordering);
    superconsole.render(&component)?;
    superconsole.finalize(&component)?;
    Ok(())
}
