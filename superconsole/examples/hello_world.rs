/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]
// We deliberately make our code stable compatible
#![cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_use_box))]

//! A basic example of using components and emitting content.

use std::{
    convert::TryInto,
    time::{Duration, Instant},
};

use superconsole::{
    components::{Component, DrawMode},
    state, Dimensions, Line, State, SuperConsole,
};
use tokio::{select, time};

/// Prints the seconds elapsed since it was created at each render loop
#[derive(Debug)]
struct Foo {
    created: Instant,
}

impl Foo {
    fn new() -> Self {
        Self {
            created: Instant::now(),
        }
    }
}

impl Component for Foo {
    /// Draws the number of seconds that have elapsed since the component was created.
    /// On a second line, draws the string "Hello world!".
    fn draw_unchecked(
        &self,
        state: &State,
        _dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Vec<Line>> {
        Ok(match mode {
            DrawMode::Final => vec![],
            DrawMode::Normal => {
                let elapsed = state.get::<Instant>().unwrap().duration_since(self.created);
                let line1 = vec![elapsed.as_secs().to_string()].try_into().unwrap();
                let line2 = vec!["Hello world!".to_owned()].try_into().unwrap();
                vec![line1, line2]
            }
        })
    }
}

/// helper method to generate lines to emit from an arbitrary word:
/// - Line 1: the string.
/// - Line 2: the string, but reversed.
/// - Line 3: "Some stuff to do with strings"
fn process_word(word: String) -> Vec<Line> {
    let echoed = word.clone();
    let reversed: String = word.chars().rev().collect();
    let line1 = vec![echoed, reversed].try_into().unwrap();
    let line2 = vec!["Some stuff to do with strings".to_owned()]
        .try_into()
        .unwrap();
    vec![line1, line2]
}

/// generates a random word using lorum ipsum at half second intervals
async fn task_that_takes_some_time() -> String {
    time::sleep(time::Duration::from_secs_f32(0.5)).await;
    lipsum::lipsum_words(1)
}

#[tokio::main]
async fn main() {
    // set up state to be used for rendering
    tokio::time::sleep(std::time::Duration::from_secs(1)).await;

    // set up future to periodically cause re-render
    let delay = Duration::from_secs(1);
    let mut interval = time::interval(delay);

    let mut renderer = SuperConsole::new(Box::new(Foo::new())).unwrap();

    // alternate between re-rendering and updating state
    loop {
        select! {
            _ = interval.tick() => {
                let time = Instant::now();
                renderer.render(&state!(&time)).unwrap();
            }
            word = task_that_takes_some_time() => {
                renderer.emit(process_word(word));
            }
        }
    }
}
