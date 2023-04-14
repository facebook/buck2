/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Example that demonstrates stylization.

use std::time::Duration;

use derive_more::Display;
use superconsole::components::Blank;
use superconsole::components::Component;
use superconsole::components::DrawMode;
use superconsole::style::style;
use superconsole::style::Color;
use superconsole::style::Stylize;
use superconsole::Dimensions;
use superconsole::Line;
use superconsole::Lines;
use superconsole::Span;
use superconsole::SuperConsole;
use tokio::time;

/// A component representing a store greeter.
struct Greeter<'a> {
    name: &'a str,
    store_name: &'a StoreName,
    customers: &'a [CustomerName],
}

#[derive(Display)]
struct StoreName(String);
#[derive(Display)]
struct CustomerName(String);

impl<'a> Component for Greeter<'a> {
    /// Prints a greeting to the current customer.
    fn draw_unchecked(&self, _dimensions: Dimensions, _mode: DrawMode) -> anyhow::Result<Lines> {
        let store_name = self.store_name;
        let customers = self.customers;
        let identification = Line(vec![
            "Hello my name is ".to_owned().italic().try_into()?,
            style(self.name.to_owned()).bold().try_into()?,
        ]);
        let mut messages = vec![identification];
        for customer_name in customers {
            let greeting = Line(vec![Span::new_styled(style(format!(
                "Welcome to {}, {}!",
                store_name, customer_name
            )))?]);
            messages.push(greeting);
        }
        Ok(Lines(messages))
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
    for i in 0usize..10usize {
        let styled = i.to_string().with(Color::Green).on(Color::Black);
        console.emit(Lines(vec![Line(vec![styled.try_into().unwrap()])]));
        let customers = (i..std::cmp::min(10, i + 2))
            .map(|x| CustomerName(people[x].to_owned()))
            .collect::<Vec<_>>();
        let store_name = StoreName(store_names[i].to_owned());

        console
            .render(&Greeter {
                name,
                store_name: &store_name,
                customers: &customers,
            })
            .unwrap();

        timer.tick().await;
    }

    // view the output before it's collapsed
    tokio::time::sleep(Duration::from_secs(1)).await;
    console.finalize(&Blank).unwrap();
}
