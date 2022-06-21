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

//! Example that demonstrates finalization.

use std::convert::TryInto;
use std::time::Duration;

use derive_more::Display;
use superconsole::components::Component;
use superconsole::components::DrawMode;
use superconsole::state;
use superconsole::Dimensions;
use superconsole::Line;
use superconsole::State;
use superconsole::SuperConsole;
use tokio::time;

/// A component representing a store greeter.
#[derive(Debug)]
struct Greeter {
    name: String,
}

#[derive(Display)]
struct StoreName(String);
#[derive(Display)]
struct CustomerName(String);

impl Component for Greeter {
    fn draw_unchecked(
        &self,
        state: &State,
        _dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Vec<Line>> {
        Ok(match mode {
            DrawMode::Normal => {
                // Prints a greeting to the current customer.
                let store_name = state.get::<StoreName>().unwrap();
                let customers = state.get::<Vec<CustomerName>>().unwrap();
                let identification = vec![format!("Hello my name is {}!", self.name)]
                    .try_into()
                    .unwrap();
                let mut messages = vec![identification];
                for customer_name in customers {
                    let greeting = vec![format!("Welcome to {}, {}!", store_name, customer_name)]
                        .try_into()
                        .unwrap();
                    messages.push(greeting);
                }
                messages
            }
            DrawMode::Final => {
                // Prints a message about the employee when he or she leaves for the day.
                let store_name = state.get::<StoreName>().unwrap();
                let total_customers = state.get::<usize>().unwrap();

                let farewell = vec![format!("{} is leaving {}", self.name, store_name)]
                    .try_into()
                    .unwrap();
                let exit_stats =
                    format!("{} greeted {} customers today", self.name, total_customers);

                vec![farewell, vec![exit_stats].try_into().unwrap()]
            }
        })
    }
}

#[tokio::main]
async fn main() {
    let mut console = SuperConsole::new(Box::new(Greeter {
        name: "Alex".to_owned(),
    }))
    .unwrap();

    let people = [
        "Joseph", "Janet", "Bob", "Christie", "Raj", "Sasha", "Rayna", "Veronika", "Russel",
        "David",
    ];
    let store_names = [
        "Target",
        "Target",
        "Target",
        "TJ",
        "TJ",
        "Walmart",
        "Wendys",
        "Wendys",
        "Uwajimaya",
        "DSW",
    ];

    let mut timer = time::interval(Duration::from_secs_f32(0.5));
    let mut last = None;
    for i in 0..(10 as usize) {
        console.emit(vec![vec![i.to_string()].try_into().unwrap()]);
        let customers = (i..std::cmp::min(10, i + 2))
            .map(|x| CustomerName(people[x].to_owned()))
            .collect::<Vec<_>>();
        let store_name = StoreName(store_names[i].to_owned());
        let correct_num = i + 1;
        let cur_state = state!(&store_name, &customers, &correct_num);
        console.render(&cur_state).unwrap();

        last = Some((store_name, customers, correct_num));

        timer.tick().await;
    }

    let (store_name, customers, correct_num) = last.unwrap();
    console
        .finalize(&state!(&store_name, &customers, &correct_num))
        .unwrap();
}
