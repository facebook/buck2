/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// If this code needs fixing, make sure you fix the README.md too!

use std::convert::Infallible;

use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Lines;
use superconsole::SuperConsole;
use superconsole::components::bordering::Bordered;
use superconsole::components::bordering::BorderedSpec;

#[derive(Debug)]
struct HelloWorld;

impl Component for HelloWorld {
    type Error = Infallible;

    fn draw_unchecked(
        &self,
        _dimensions: Dimensions,
        _mode: DrawMode,
    ) -> Result<Lines, Infallible> {
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
