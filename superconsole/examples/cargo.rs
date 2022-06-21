/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::thread::sleep;
use std::time::Duration;

use superconsole::style::Stylize;
use superconsole::Component;
use superconsole::Span;
use superconsole::SuperConsole;

const CRATES: &str = include_str!("cargo/crates.txt");
const WIDTH: usize = "=======>                  ".len() - 1;

#[derive(Debug)]
struct LoadingBar<'a>(pub Vec<&'a str>);

impl<'a> Component for LoadingBar<'a> {
    fn draw_unchecked(
        &self,
        state: &superconsole::State,
        _dimensions: superconsole::Dimensions,
        mode: superconsole::DrawMode,
    ) -> anyhow::Result<superconsole::Lines> {
        let res = match mode {
            superconsole::DrawMode::Normal => {
                const BUILDING: &str = "   Building ";
                let iteration = state.get::<usize>()?;
                let percentage = *iteration as f64 / self.0.len() as f64;
                let amount = (percentage * WIDTH as f64).ceil() as usize;

                let building = Span::new_styled(BUILDING.to_owned().cyan().bold())?;
                let loading_bar = format!(
                    "[{test:=>bar_amt$}{test2:padding_amt$}] {}/{}: ...",
                    iteration,
                    self.0.len(),
                    test = ">",
                    test2 = "",
                    bar_amt = amount,
                    padding_amt = WIDTH - amount,
                );
                let loading = Span::new_unstyled(loading_bar)?;
                superconsole::line!(building, loading)
            }
            superconsole::DrawMode::Final => {
                const FINISHED: &str = "   Finished ";
                let finished = Span::new_styled(FINISHED.to_owned().green().bold())?;
                const COMPLETION: &str = "dev [unoptimized + debuginfo] target(s) in 14.45s";
                superconsole::line!(finished, Span::new_unstyled(COMPLETION)?)
            }
        };

        Ok(vec![res])
    }
}

fn main() {
    let crates: Vec<_> = CRATES
        .lines()
        .map(|line| line.trim().split_once(char::is_whitespace).unwrap().1)
        .collect();
    let count = crates.len();

    let loading_bar = LoadingBar(crates.clone());
    let mut superconsole = SuperConsole::new(Box::new(loading_bar)).unwrap();

    for (i, c) in crates.into_iter().enumerate() {
        let building = Span::new_styled("  Compiling ".to_owned().green().bold()).unwrap();
        superconsole.emit(vec![superconsole::line!(
            building,
            Span::new_unstyled(c).unwrap()
        )]);
        superconsole.render(&superconsole::state!(&i)).unwrap();
        sleep(Duration::from_secs_f64(0.2));
    }

    superconsole
        .finalize(&superconsole::state!(&count))
        .unwrap();
}
