/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Example that demonstrates finalization.

use std::time::Duration;

use derive_more::Display;
use superconsole::components::Component;
use superconsole::components::DrawMode;
use superconsole::Dimensions;
use superconsole::Lines;
use superconsole::SuperConsole;
use tokio::time;

/// A component representing a store greeter.
struct Greeter<'a> {
    name: &'a str,
    store_name: &'a StoreName,
    customers: &'a [CustomerName],
    correct_num: usize,
}

#[derive(Display)]
struct StoreName(String);
#[derive(Display)]
struct CustomerName(String);

impl<'a> Component for Greeter<'a> {
    fn draw_unchecked(&self, _dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        Ok(match mode {
            DrawMode::Normal => {
                // Prints a greeting to the current customer.
                let store_name = self.store_name;
                let customers = self.customers;
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
                Lines(messages)
            }
            DrawMode::Final => {
                // Prints a message about the employee when he or she leaves for the day.
                let store_name = self.store_name;
                let total_customers = self.correct_num;

                let farewell = vec![format!("{} is leaving {}", self.name, store_name)]
                    .try_into()
                    .unwrap();
                let exit_stats =
                    format!("{} greeted {} customers today", self.name, total_customers);

                Lines(vec![farewell, vec![exit_stats].try_into().unwrap()])
            }
        })
    }
}

#[tokio::main]
async fn main() {
    let name = "Alex";
    let mut console = SuperConsole::new().unwrap();

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
    for (i, store_name) in store_names.iter().enumerate() {
        console.emit(Lines(vec![vec![i.to_string()].try_into().unwrap()]));
        let customers = (i..std::cmp::min(10, i + 2))
            .map(|x| CustomerName(people[x].to_owned()))
            .collect::<Vec<_>>();
        let store_name = StoreName((*store_name).to_owned());
        let correct_num = i + 1;
        console
            .render(&Greeter {
                name,
                store_name: &store_name,
                customers: &customers,
                correct_num,
            })
            .unwrap();

        last = Some((store_name, customers, correct_num));

        timer.tick().await;
    }

    let (store_name, customers, correct_num) = last.unwrap();
    console
        .finalize(&Greeter {
            name,
            store_name: &store_name,
            customers: &customers,
            correct_num,
        })
        .unwrap();
}
