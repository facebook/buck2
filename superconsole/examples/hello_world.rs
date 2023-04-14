/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A basic example of using components and emitting content.

use std::time::Duration;
use std::time::Instant;

use superconsole::components::Component;
use superconsole::components::DrawMode;
use superconsole::Dimensions;
use superconsole::Line;
use superconsole::Lines;
use superconsole::SuperConsole;
use tokio::select;
use tokio::time;

/// Prints the seconds elapsed since it was created at each render loop
#[derive(Debug)]
struct Foo {
    created: Instant,
    now: Instant,
}

impl Component for Foo {
    /// Draws the number of seconds that have elapsed since the component was created.
    /// On a second line, draws the string "Hello world!".
    fn draw_unchecked(&self, _dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        Ok(match mode {
            DrawMode::Final => Lines::new(),
            DrawMode::Normal => {
                let elapsed = self.now.duration_since(self.created);
                let line1 = vec![elapsed.as_secs().to_string()].try_into().unwrap();
                let line2 = vec!["Hello world!".to_owned()].try_into().unwrap();
                Lines(vec![line1, line2])
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
    "hello world".to_owned()
}

#[tokio::main]
async fn main() {
    // set up state to be used for rendering
    tokio::time::sleep(std::time::Duration::from_secs(1)).await;

    // set up future to periodically cause re-render
    let delay = Duration::from_secs(1);
    let mut interval = time::interval(delay);

    let created = Instant::now();
    let mut renderer = SuperConsole::new().unwrap();

    // alternate between re-rendering and updating state
    loop {
        select! {
            _ = interval.tick() => {
                renderer.render(&Foo {
                    created,
                    now: Instant::now(),
                }).unwrap();
            }
            word = task_that_takes_some_time() => {
                renderer.emit(Lines(process_word(word)));
            }
        }
    }
}
