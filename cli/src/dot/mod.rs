/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A very limited interface for writing dot files (see <http://www.graphviz.org/doc/info/lang.html>)
//!
//! Has a lot less features than <https://crates.io/crates/dot> or <https://crates.io/crates/tabbycat>,
//! but it's easier for us to match buck1's output with this simple implementation.
//!
// TODO(cjhopman): while the `dot` crate is probably too opinionated, `tabbycat` looks nice and is
// lower level so gives a lot of control (including control over ordering of node/edge statements).
// It looks like we could use that, but it mostly would just handle the actual writing of the
// data in the right format and maybe escaping. It's not been imported to tp2 so we implement it
// ourselves for now.

use std::collections::hash_map::Entry::Occupied;
use std::collections::hash_map::Entry::Vacant;
use std::collections::HashMap;
use std::fmt::Display;
use std::io::Write;

use indexmap::IndexMap;
use once_cell::sync::Lazy;
use regex::Regex;

pub mod targets;

#[derive(Default, Debug)]
pub(crate) struct DotNodeAttrs {
    pub style: Option<String>,
    pub color: Option<String>,
    pub label: Option<String>,
    pub extra: IndexMap<String, String>,
}

impl Display for DotNodeAttrs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let style = self.style.as_ref().map(|v| ("style", v));
        let color = self.color.as_ref().map(|v| ("color", v));
        let label = self.label.as_ref().map(|v| ("label", v));

        for (i, (key, value)) in style
            .into_iter()
            .chain(color.into_iter())
            .chain(label.into_iter())
            .chain(self.extra.iter().map(|(l, r)| (l.as_str(), r)))
            .enumerate()
        {
            if i != 0 {
                f.write_str(",")?;
            }
            write!(f, "{}={}", escape_id(key), escape_id(value))?;
        }
        Ok(())
    }
}

/// A node in the graph.
pub(crate) trait DotNode {
    fn attrs(&self) -> anyhow::Result<DotNodeAttrs>;
    fn id(&self) -> String;
}

/// Represents a directed edge between two nodes, identified by their id.
pub(crate) struct DotEdge<'a> {
    from: &'a str,
    to: &'a str,
}

pub(crate) trait DotDigraph<'a> {
    type Node: DotNode;

    fn name(&self) -> &str;

    fn for_each_node<F: FnMut(&Self::Node) -> anyhow::Result<()>>(
        &'a self,
        f: F,
    ) -> anyhow::Result<()>;
    fn for_each_edge<F: FnMut(&DotEdge) -> anyhow::Result<()>>(
        &'a self,
        node: &Self::Node,
        f: F,
    ) -> anyhow::Result<()>;
}

/// ids in dot format need to have the '"' escaped.
///
/// From
/// ```ignore
///  An ID is one of the following:
///  - Any string of alphabetic ([a-zA-Z\200-\377]) characters, underscores ('_') or digits([0-9]), not beginning with a digit;
///  - a numeral [-]?(.[0-9]⁺ | [0-9]⁺(.[0-9]*)? );
///  - any double-quoted string ("...") possibly containing escaped quotes (\")¹;
///  - an HTML string (<...>).
///
/// We support (approximately) the first two forms and then anything else gets quoted and escaped as the third form.
fn escape_id(value: &str) -> String {
    static RE_STRING: Lazy<Regex> = Lazy::new(|| Regex::new("^[a-zA-Z_][a-zA-Z_0-9]*$").unwrap());
    static RE_NUMBER: Lazy<Regex> =
        Lazy::new(|| Regex::new("^-?(.[0-9]+ | [0-9]+.[0-9]*)$").unwrap());

    if RE_STRING.is_match(value) || RE_NUMBER.is_match(value) {
        return value.to_owned();
    }

    format!("\"{}\"", value.replace('"', "\\\""))
}

pub(crate) struct Dot {}

impl Dot {
    pub(crate) fn render<'a, T: DotDigraph<'a>, W: Write>(
        graph: &'a T,
        mut w: W,
    ) -> anyhow::Result<()> {
        writeln!(w, "digraph {} {{", graph.name())?;
        graph.for_each_node(|node| {
            let attrs = node.attrs()?;
            writeln!(w, "  {} [{}];", escape_id(&node.id()), attrs)?;
            graph.for_each_edge(node, |edge| {
                writeln!(w, "  {} -> {};", escape_id(edge.from), escape_id(edge.to))?;
                Ok(())
            })?;
            Ok(())
        })?;
        writeln!(w, "}}")?;
        Ok(())
    }
}

pub(crate) struct DotCompact {}

impl DotCompact {
    pub(crate) fn render<'a, T: DotDigraph<'a>, W: Write>(
        graph: &'a T,
        mut w: W,
    ) -> anyhow::Result<()> {
        writeln!(w, "digraph {} {{", graph.name())?;

        let mut next_id: u32 = 0;
        let mut lookup_numeric_id: HashMap<String, u32> = HashMap::new();

        let mut name_to_number = |node_name: &str| -> u32 {
            match lookup_numeric_id.entry(node_name.to_owned()) {
                Vacant(entry) => {
                    next_id += 1;
                    entry.insert(next_id);
                    next_id
                }
                Occupied(entry) => *entry.get(),
            }
        };

        graph.for_each_node(|node| {
            let attrs = node.attrs()?;
            let node_name = &escape_id(&node.id());
            writeln!(
                w,
                "  {} [{},label={}];",
                name_to_number(node_name),
                attrs,
                escape_id(&node.id())
            )?;
            graph.for_each_edge(node, |edge| {
                writeln!(
                    w,
                    "  {} -> {};",
                    name_to_number(&escape_id(edge.from)),
                    name_to_number(&escape_id(edge.to))
                )?;
                Ok(())
            })?;
            Ok(())
        })?;
        writeln!(w, "}}")?;
        Ok(())
    }
}
