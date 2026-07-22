/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Sorts list arguments for allowlisted fields in rule calls.
//!
//! When a rule call has a keyword argument whose name is in the allowlist
//! (e.g., `deps`, `srcs`, `visibility`), and its value is a list literal,
//! the list elements are sorted alphabetically. String elements and
//! `external_deps`-style tuples (`(project, version, name)`, where `version`
//! and `name` may be `None`) are sortable; any other element is preserved in
//! place and acts as a barrier between sortable runs.
//!
//! Sorting can be disabled for specific macro/arg combinations via the
//! blocklist (e.g., `genrule.srcs` where order may be significant).
//!
//! ```starlark
//! # Before:
//! rust_library(
//!     name = "foo",
//!     deps = [":z", ":a", ":m"],
//! )
//!
//! # After:
//! rust_library(
//!     name = "foo",
//!     deps = [":a", ":m", ":z"],
//! )
//! ```

use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::HashSet;

use ruff_python_ast::Expr;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprList;
use ruff_python_ast::ExprTuple;
use ruff_python_ast::Operator;
use ruff_python_ast::Stmt;
use ruff_python_ast::visitor::Visitor;
use ruff_python_ast::visitor::walk_expr;
use ruff_python_ast::visitor::walk_stmt;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

use super::parsed_module::Edit;
use super::parsed_module::ParsedModule;
use super::utils::block_ranges_valid;
use super::utils::block_with_trailing_comma;
use super::utils::compute_block_ranges;
use super::utils::find_line_end;
use super::utils::line_start;
use super::utils::offset_past_newline;
use crate::autofixes::utils::find_ignore_ascii_case;
use crate::config::Config;

/// Sort key for an `external_deps`-style tuple `(project, version, name)`
/// (`version`/`name` may be `None`), or `None` if it isn't safe to reorder.
///
/// Ordered by project, then name, then version — matching
/// `fbcode/scripts/orvid/target_format.d`. Encoded as `project:name:version` so
/// the existing Buildifier comparator (which splits on `:`/`.`) yields that order.
fn tuple_sort_key(tuple: &ExprTuple) -> Option<String> {
    // Non-empty string keys by value; `None`/missing keys as empty. Non-string
    // elements and empty string literals yield `None` (tuple becomes a barrier):
    // an empty literal would collide with a `None`/missing position and risk a
    // silent dedup of distinct tuples.
    let part = |idx: usize| match tuple.elts.get(idx) {
        Some(Expr::StringLiteral(s)) => {
            let value = s.value.to_str();
            (!value.is_empty()).then_some(value)
        }
        None | Some(Expr::NoneLiteral(_)) => Some(""),
        Some(_) => None,
    };
    // A valid entry has at most three positions; the key only encodes three, so
    // sorting a longer tuple could drop a distinct element as a "duplicate".
    if tuple.elts.len() > 3 {
        return None;
    }
    let project = part(0)?;
    let version = part(1)?;
    let name = part(2)?;
    // A `:` inside a component makes the joined key ambiguous (e.g. `("a", None,
    // "b:c")` and `("a:b", None, "c")` both yield `a:b:c:`), risking silent
    // dedup. `.` is allowed so versions like `1.2.3` stay sortable.
    if [project, version, name].iter().any(|c| c.contains(':')) {
        return None;
    }
    Some(format!("{project}:{name}:{version}"))
}

/// The sort key for a list element, or `None` when the element isn't sortable
/// (and therefore acts as a barrier between sortable runs).
///
/// String literals sort by their value; `external_deps`-style tuples sort by
/// [`tuple_sort_key`].
fn element_sort_key(expr: &Expr) -> Option<Cow<'_, str>> {
    match expr {
        Expr::StringLiteral(s) => Some(Cow::Borrowed(s.value.to_str())),
        Expr::Tuple(t) => tuple_sort_key(t).map(Cow::Owned),
        _ => None,
    }
}

fn string_sort_phase(value: &str) -> u8 {
    match value {
        _ if value.starts_with(':') => 1,
        _ if value.starts_with("//") => 2,
        _ if value.starts_with('@') => 3,
        _ => 0,
    }
}

/// Compare two strings as Buildifier does within the path-component sort
/// phase: split on either `:` or `.` and compare the resulting components
/// lexicographically. We walk the strings byte-by-byte so the comparison
/// runs in O(len) without allocating temporary `String`s — important since
/// `Vec::sort_by` invokes this O(n log n) times per list, and the sort is
/// on the hot path for every BUILD/TARGETS file.
fn compare_buildifier_split_parts(a: &str, b: &str) -> Ordering {
    let is_separator = |c| c == ':' || c == '.';
    let mut a_parts = a.split(is_separator);
    let mut b_parts = b.split(is_separator);

    loop {
        match (a_parts.next(), b_parts.next()) {
            (Some(a_part), Some(b_part)) if a_part != b_part => {
                return a_part.cmp(b_part);
            }
            (Some(_), Some(_)) => {}
            (Some(_), None) => return Ordering::Greater,
            (None, Some(_)) => return Ordering::Less,
            (None, None) => return Ordering::Equal,
        }
    }
}

fn compare_buildifier_string_values(a: &str, b: &str) -> Ordering {
    string_sort_phase(a)
        .cmp(&string_sort_phase(b))
        .then_with(|| compare_buildifier_split_parts(a, b))
        .then_with(|| a.cmp(b))
}

fn sorted_unique_indices(values: &[Cow<str>]) -> Vec<usize> {
    // `Vec::sort_by` is stable, so equal keys preserve insertion order.
    // No explicit tiebreaker on the original index is needed.
    let mut indices: Vec<usize> = (0..values.len()).collect();
    indices
        .sort_by(|&a, &b| compare_buildifier_string_values(values[a].as_ref(), values[b].as_ref()));
    indices.dedup_by(|a, b| values[*a] == values[*b]);
    indices
}

fn is_unchanged_unique_order(indices: &[usize], original_len: usize) -> bool {
    indices.len() == original_len && indices.iter().enumerate().all(|(i, &j)| i == j)
}

/// Check if source text contains a comment that isn't a "keep sorted" directive.
fn has_non_directive_comment(content: &str) -> bool {
    content.contains('#') && find_ignore_ascii_case(content, "keep sorted").is_none()
}

fn has_line_comment(source: &str, range: TextRange) -> bool {
    source[range]
        .lines()
        .any(|line| line.trim_start().starts_with('#'))
}

fn has_do_not_sort_directive(module: &ParsedModule, range: TextRange) -> bool {
    module
        .comments_in_range(range)
        .any(|comment| find_ignore_ascii_case(comment, "do not sort").is_some())
}

fn has_do_not_sort_on_first_element(module: &ParsedModule, list: &ExprList) -> bool {
    let Some(first_elem) = list.elts.first() else {
        return false;
    };

    let range = TextRange::new(
        list.range().start() + TextSize::from(1),
        first_elem.range().start(),
    );
    has_do_not_sort_directive(module, range)
}

/// Force-sorting a mixed list can reorder only a subset of elements and break
/// call-specific invariants (for example, `conditional_deps` total ordering).
/// Only allow force sorting when every element is individually sortable (a
/// string literal or an `external_deps`-style tuple).
fn can_force_sort_list(list: &ExprList) -> bool {
    list.elts.iter().all(|elt| element_sort_key(elt).is_some())
}

/// Check if a list needs sorting and return edits.
///
/// Comments between elements act as group separators. Elements within each
/// group are sorted independently using line-based block ranges, so trailing
/// inline comments (e.g., `# @manual`) move with their element.
fn collect_sorted_elements_with_comments(
    list: &ExprList,
    source: &str,
    force_sort: bool,
) -> Option<Vec<(TextRange, String)>> {
    let elts = &list.elts;
    if elts.len() < 2 {
        return None;
    }

    // Check if all elements are on the same line (single-line list)
    let first_line_end = find_line_end(source, elts[0].range().start());
    let last_elt = &elts[elts.len() - 1];
    if last_elt.range().end() <= first_line_end {
        // Single-line list: no comments to worry about
        return collect_sorted_elements(list, source);
    }

    if !force_sort && has_line_comment(source, list.range()) {
        return collect_deduplicated_elements(list, source);
    }

    // Check if there are any comments (excluding "keep sorted" directives)
    let has_comments = elts.iter().enumerate().any(|(i, elt)| {
        let content_start = if i == 0 {
            let bracket_end = find_line_end(source, list.range().start());
            offset_past_newline(source, bracket_end).to_usize()
        } else {
            let prev_line_end = find_line_end(source, elts[i - 1].range().end());
            offset_past_newline(source, prev_line_end).to_usize()
        };
        let elt_start = elt.range().start().to_usize();
        if content_start > elt_start {
            return false;
        }
        let content = &source[content_start..elt_start];
        has_non_directive_comment(content)
    });

    // If no comments, sort the entire list using block ranges
    if !has_comments {
        return sort_multiline_list(list, elts, source);
    }

    // Comments exist — they act as group separators.
    // Sort within each group independently using block ranges.
    let mut all_edits = Vec::new();
    let mut i = 0;

    while i < elts.len() {
        if element_sort_key(&elts[i]).is_none() {
            i += 1;
            continue;
        }

        // Find contiguous run of string elements
        let run_start = i;
        while i < elts.len() && element_sort_key(&elts[i]).is_some() {
            i += 1;
        }
        let run_end = i;

        if run_end - run_start < 2 {
            continue;
        }

        // Break this run into groups at comment boundaries.
        // A comment on its OWN line between elements starts a new group.
        // Trailing inline comments (on the same line as an element) are NOT
        // group separators — they travel with the element during sorting.
        let mut group_start = run_start;
        for j in (run_start + 1)..run_end {
            // Start after the previous element's line end so trailing inline
            // comments (e.g., `":z",  # z inline`) are excluded from the check.
            let prev_line_end = find_line_end(source, elts[j - 1].range().end());
            let after_prev_line = offset_past_newline(source, prev_line_end).to_usize();
            let cur_start = elts[j].range().start().to_usize();
            // If two elements are on the same line, there's no inter-line content
            // to check for group-separating comments — skip.
            if after_prev_line > cur_start {
                continue;
            }
            let content = &source[after_prev_line..cur_start];
            if has_non_directive_comment(content) {
                if let Some(edits) = sort_element_group(list, elts, source, group_start, j) {
                    all_edits.extend(edits);
                }
                group_start = j;
            }
        }
        // Sort the last group
        if let Some(edits) = sort_element_group(list, elts, source, group_start, run_end) {
            all_edits.extend(edits);
        }
    }

    if all_edits.is_empty() {
        None
    } else {
        Some(all_edits)
    }
}

/// Sort all elements in a multiline list using block ranges (no group splitting).
fn sort_multiline_list(
    list: &ExprList,
    elts: &[Expr],
    source: &str,
) -> Option<Vec<(TextRange, String)>> {
    let mut all_edits = Vec::new();
    let mut i = 0;

    while i < elts.len() {
        if element_sort_key(&elts[i]).is_none() {
            i += 1;
            continue;
        }

        let run_start = i;
        while i < elts.len() && element_sort_key(&elts[i]).is_some() {
            i += 1;
        }
        let run_end = i;

        if run_end - run_start < 2 {
            continue;
        }

        if let Some(edits) = sort_element_group(list, elts, source, run_start, run_end) {
            all_edits.extend(edits);
        }
    }

    if all_edits.is_empty() {
        None
    } else {
        Some(all_edits)
    }
}

/// Compute the block start for the first element in a group, handling
/// `# keep sorted` directives that should stay in place.
fn first_element_block_start(
    list: &ExprList,
    elts: &[Expr],
    source: &str,
    group_start: usize,
) -> TextSize {
    if group_start == 0 {
        // First element in list: start after the '[' line
        let bracket_line_end = find_line_end(source, list.range().start());
        let after_bracket = offset_past_newline(source, bracket_line_end);

        // Check for content between bracket line and first element
        let first_elt_start = elts[0].range().start();
        if after_bracket > first_elt_start {
            // First element is on the same line as '[' — use element's line start
            return TextSize::from(line_start(source, first_elt_start.to_usize()) as u32);
        }
        let between = &source[after_bracket.to_usize()..first_elt_start.to_usize()];

        if let Some(ks_pos) = find_ignore_ascii_case(between, "keep sorted") {
            // Keep sorted directive: start after it so it stays pinned
            if let Some(newline_after) = between[ks_pos..].find('\n') {
                let after_ks = after_bracket + TextSize::from((ks_pos + newline_after + 1) as u32);
                // Check if there's ALSO a group header comment after the keep-sorted
                let remaining = &source[after_ks.to_usize()..elts[0].range().start().to_usize()];
                if has_non_directive_comment(remaining) {
                    let elt_start = elts[0].range().start().to_usize();
                    return TextSize::from(line_start(source, elt_start) as u32);
                }
                return after_ks;
            }
        }

        if has_non_directive_comment(between) {
            // Group header comment (not keep-sorted): don't include it in the
            // first element's block. Start from the element's own line instead.
            let elt_start = elts[0].range().start().to_usize();
            return TextSize::from(line_start(source, elt_start) as u32);
        }

        after_bracket
    } else {
        // First element in group but not first in list.
        // There's a group header comment between the previous group's last
        // element and this element. Start from this element's own line so the
        // header comment stays in place.
        let elt_start = elts[group_start].range().start().to_usize();
        TextSize::from(line_start(source, elt_start) as u32)
    }
}

/// Sort a contiguous group of string elements using line-based block ranges.
///
/// Each element's "block" spans from after the previous element's line end to
/// the end of this element's line. Leading comments and trailing inline comments
/// move together with their element during sorting.
fn sort_element_group(
    list: &ExprList,
    elts: &[Expr],
    source: &str,
    group_start: usize,
    group_end: usize,
) -> Option<Vec<(TextRange, String)>> {
    if group_end - group_start < 2 {
        return None;
    }

    let sort_keys: Vec<Cow<str>> = (group_start..group_end)
        .map(|idx| {
            element_sort_key(&elts[idx])
                .expect("group elements are pre-filtered to sortable elements")
        })
        .collect();

    let sorted_indices = sorted_unique_indices(&sort_keys);

    if is_unchanged_unique_order(&sorted_indices, sort_keys.len()) {
        return None;
    }

    // Compute line-based block ranges for each element in the group
    let first_start = first_element_block_start(list, elts, source, group_start);
    let element_ends: Vec<TextSize> = (group_start..group_end)
        .map(|idx| elts[idx].range().end())
        .collect();
    let block_ranges = compute_block_ranges(source, first_start, &element_ends);

    if !block_ranges_valid(&block_ranges, source) {
        // Safety fallback: use simple element-only swapping
        return collect_sorted_elements_simple(elts, source, group_start, group_end);
    }

    let group_range = TextRange::new(
        block_ranges[0].start(),
        block_ranges[block_ranges.len() - 1].end(),
    );
    let group_size = sorted_indices.len();
    let mut replacement = String::new();
    for (new_pos, &old_pos) in sorted_indices.iter().enumerate() {
        let source_range = block_ranges[old_pos];
        let block = &source[source_range];

        // If moving a block to a non-last position, ensure it has a trailing comma.
        // Without this, the last element (which may lack a comma) would cause
        // implicit string concatenation when placed before another element.
        if new_pos < group_size - 1 {
            let elt = &elts[group_start + old_pos];
            match block_with_trailing_comma(source, source_range, elt.range().end()) {
                Some(block_text) => replacement.push_str(&block_text),
                // For malformed/edge block boundaries (e.g., multiple elements on
                // one line), preserve behavior by falling back to element-only swaps.
                None => {
                    return collect_sorted_elements_simple(elts, source, group_start, group_end);
                }
            }
        } else {
            replacement.push_str(block);
        }
    }

    Some(vec![(group_range, replacement)])
}

/// Check if a list needs sorting and return sorted ranges and values.
/// Simple version that only swaps element values, not preserving comments.
fn collect_sorted_elements(list: &ExprList, source: &str) -> Option<Vec<(TextRange, String)>> {
    let elts = &list.elts;
    if elts.len() < 2 {
        return None;
    }

    let mut edits = Vec::new();
    let mut i = 0;

    while i < elts.len() {
        if element_sort_key(&elts[i]).is_none() {
            i += 1;
            continue;
        }

        let start = i;
        while i < elts.len() && element_sort_key(&elts[i]).is_some() {
            i += 1;
        }

        if let Some(run_edits) = collect_sorted_elements_simple(elts, source, start, i) {
            edits.extend(run_edits);
        }
    }

    if edits.is_empty() { None } else { Some(edits) }
}

/// Sort a contiguous run of string elements using AST ranges only (no comments).
fn collect_sorted_elements_simple(
    elts: &[Expr],
    source: &str,
    run_start: usize,
    run_end: usize,
) -> Option<Vec<(TextRange, String)>> {
    if run_end - run_start < 2 {
        return None;
    }

    let sort_keys: Vec<Cow<str>> = (run_start..run_end)
        .map(|idx| element_sort_key(&elts[idx]).unwrap_or(Cow::Borrowed("")))
        .collect();
    let indices = sorted_unique_indices(&sort_keys);

    if is_unchanged_unique_order(&indices, sort_keys.len()) {
        return None;
    }

    let replacement = indices
        .iter()
        .map(|&old_pos| source[elts[run_start + old_pos].range()].to_string())
        .collect::<Vec<_>>()
        .join(", ");
    let range = TextRange::new(
        elts[run_start].range().start(),
        elts[run_end - 1].range().end(),
    );

    Some(vec![(range, replacement)])
}

fn single_line_duplicate_range(list: &ExprList, elt: &Expr, source: &str) -> TextRange {
    let mut start = elt.range().start().to_usize();
    let end = elt.range().end().to_usize();
    let list_start = list.range().start().to_usize() + 1;

    // Always consume the preceding comma and any whitespace between it and
    // the duplicate element. Duplicates are always at idx >= 1 (the first
    // occurrence is kept by `collect_deduplicated_elements`), so a preceding
    // comma always exists. Consuming on the same side ensures adjacent
    // duplicate ranges don't overlap, which would otherwise cause panics
    // when edits are applied in reverse start-position order.
    if let Some(comma_offset) = source[list_start..start].rfind(',') {
        start = list_start + comma_offset;
    }

    TextRange::new(TextSize::from(start as u32), TextSize::from(end as u32))
}

fn multiline_duplicate_range(
    list: &ExprList,
    elts: &[Expr],
    source: &str,
    idx: usize,
) -> TextRange {
    let start = if idx == 0 {
        first_element_block_start(list, elts, source, 0)
    } else {
        offset_past_newline(source, find_line_end(source, elts[idx - 1].range().end()))
    };
    let end = offset_past_newline(source, find_line_end(source, elts[idx].range().end()));
    TextRange::new(start, end)
}

fn collect_deduplicated_elements(
    list: &ExprList,
    source: &str,
) -> Option<Vec<(TextRange, String)>> {
    let elts = &list.elts;
    if elts.len() < 2 {
        return None;
    }

    let is_single_line =
        elts[elts.len() - 1].range().end() <= find_line_end(source, list.range().start());
    let mut seen = HashSet::new();
    let mut edits = Vec::new();

    for (idx, elt) in elts.iter().enumerate() {
        let Some(value) = element_sort_key(elt) else {
            continue;
        };
        if seen.insert(value) {
            continue;
        }

        let range = if is_single_line {
            single_line_duplicate_range(list, elt, source)
        } else {
            multiline_duplicate_range(list, elts, source, idx)
        };
        edits.push((range, String::new()));
    }

    if edits.is_empty() { None } else { Some(edits) }
}

/// Extract the function name from a Call expression.
fn call_func_name(call: &ExprCall) -> Option<&str> {
    match call.func.as_ref() {
        Expr::Name(n) => Some(n.id.as_str()),
        Expr::Attribute(attr) => Some(attr.attr.as_str()),
        _ => None,
    }
}

/// Check if a range contains a "keep sorted" directive in any comment.
fn has_keep_sorted_directive(module: &ParsedModule, range: TextRange) -> bool {
    module
        .comments_in_range(range)
        .any(|comment| find_ignore_ascii_case(comment, "keep sorted").is_some())
}

/// Check if a list has the `# keep sorted` directive before the first element.
fn has_keep_sorted_on_first_element(module: &ParsedModule, list: &ExprList) -> bool {
    if list.elts.is_empty() {
        return false;
    }

    let first_elem = &list.elts[0];
    let range = TextRange::new(
        list.range().start() + TextSize::from(1),
        first_elem.range().start(),
    );

    has_keep_sorted_directive(module, range)
}

/// Visitor that collects edits for sorting list arguments.
struct ListArgSorter<'a, 'c> {
    module: &'a ParsedModule<'a>,
    config: &'c Config,
    sort_rule_args: bool,
    edits: Vec<Edit>,
    /// Stack of function names for nested calls.
    call_stack: Vec<Option<String>>,
    /// End offset of the previous statement for statement-level directive checks.
    prev_stmt_end: TextSize,
    /// Track lists that were already processed to avoid duplicate edits.
    handled_lists: HashSet<TextRange>,
}

impl<'a, 'c> ListArgSorter<'a, 'c> {
    fn new(module: &'a ParsedModule<'a>, config: &'c Config, sort_rule_args: bool) -> Self {
        Self {
            module,
            config,
            sort_rule_args,
            edits: Vec::new(),
            call_stack: Vec::new(),
            prev_stmt_end: TextSize::from(0),
            handled_lists: HashSet::new(),
        }
    }

    fn current_macro_name(&self) -> Option<&str> {
        self.call_stack.last().and_then(|s| s.as_deref())
    }

    fn should_sort_arg(&self, arg_name: &str) -> bool {
        // Check allowlist
        if !self.config.sortable_args().contains(arg_name) {
            return false;
        }

        // Check blocklist
        if let Some(macro_name) = self.current_macro_name() {
            let blocklist_key = format!("{}.{}", macro_name, arg_name);
            if self.config.sortable_blocklist().contains(&blocklist_key) {
                return false;
            }
        }

        true
    }

    fn deduplicate_list(&mut self, list: &ExprList) {
        if let Some(element_edits) = collect_deduplicated_elements(list, self.module.source()) {
            for (range, replacement) in element_edits {
                self.edits.push(Edit::new(range, replacement));
            }
        }
    }

    /// Sort a list and add edits, preserving leading comments.
    fn sort_list(&mut self, list: &ExprList, force_sort: bool) {
        if !self.handled_lists.insert(list.range()) {
            return;
        }

        if has_do_not_sort_on_first_element(self.module, list) {
            self.deduplicate_list(list);
            return;
        }

        let force_sort = force_sort && can_force_sort_list(list);
        if let Some(element_edits) =
            collect_sorted_elements_with_comments(list, self.module.source(), force_sort)
        {
            for (range, replacement) in element_edits {
                self.edits.push(Edit::new(range, replacement));
            }
        }
    }

    /// Recursively find and sort list literals within an expression.
    ///
    /// This handles:
    /// - Direct list literals: `[":z", ":a"]`
    /// - BinOp with Add: `[":z", ":a"] + [":m"]` (sort each list operand)
    /// - select() dict values: `select({"key": [":z", ":a"]})` (sort each list value)
    fn sort_lists_in_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::List(list) => {
                let force_sort = has_keep_sorted_on_first_element(self.module, list);
                self.sort_list(list, force_sort);
            }
            Expr::BinOp(binop) if matches!(binop.op, Operator::Add) => {
                self.sort_lists_in_expr(&binop.left);
                self.sort_lists_in_expr(&binop.right);
            }
            Expr::Call(call) => {
                // Handle select({...}) - sort list values inside the dict
                if let Some(name) = call_func_name(call) {
                    if name == "select" {
                        if let Some(Expr::Dict(dict)) = call.arguments.args.first() {
                            for item in &dict.items {
                                self.sort_lists_in_expr(&item.value);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn has_keep_sorted_before_stmt(&self, stmt: &Stmt) -> bool {
        let range = TextRange::new(self.prev_stmt_end, stmt.range().start());
        has_keep_sorted_directive(self.module, range)
    }
}

impl<'a, 'c> Visitor<'a> for ListArgSorter<'a, 'c> {
    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        if let Stmt::Assign(assign) = stmt
            && let Expr::List(list) = assign.value.as_ref()
            && self.has_keep_sorted_before_stmt(stmt)
        {
            self.sort_list(list, true);
        }

        if let Stmt::AnnAssign(ann_assign) = stmt
            && let Some(value) = &ann_assign.value
            && let Expr::List(list) = value.as_ref()
            && self.has_keep_sorted_before_stmt(stmt)
        {
            self.sort_list(list, true);
        }

        walk_stmt(self, stmt);
        self.prev_stmt_end = stmt.range().end();
    }

    fn visit_expr(&mut self, expr: &'a Expr) {
        if let Expr::Call(call) = expr {
            let func_name = call_func_name(call).map(String::from);
            self.call_stack.push(func_name);

            // Process keyword arguments
            if self.sort_rule_args {
                for kw in &call.arguments.keywords {
                    if let Some(arg_name) = &kw.arg
                        && self.should_sort_arg(arg_name.as_str())
                    {
                        if has_do_not_sort_directive(self.module, kw.range()) {
                            if let Expr::List(list) = &kw.value {
                                self.deduplicate_list(list);
                            }
                        } else {
                            self.sort_lists_in_expr(&kw.value);
                        }
                    }
                }
            }

            // Visit children (including nested calls)
            walk_expr(self, expr);

            self.call_stack.pop();
        } else if let Expr::List(list) = expr {
            if has_keep_sorted_on_first_element(self.module, list) {
                self.sort_list(list, true);
            }
            walk_expr(self, expr);
        } else {
            walk_expr(self, expr);
        }
    }
}

/// Collect edits that sort list arguments according to config.
///
/// `sort_rule_args` gates sorting of allowlisted rule arguments (`deps`,
/// `srcs`, ...). When false (e.g. for `.bzl` files), only explicit
/// `# keep sorted` lists are sorted, matching Buildifier.
pub(crate) fn collect_edits(
    module: &ParsedModule,
    config: &Config,
    sort_rule_args: bool,
) -> Vec<Edit> {
    let source = module.source();
    if source.is_empty() {
        return Vec::new();
    }

    let mut sorter = ListArgSorter::new(module, config, sort_rule_args);
    for stmt in module.stmts() {
        sorter.visit_stmt(stmt);
    }

    sorter.edits
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;
    use std::collections::HashSet;

    use indoc::indoc;

    use super::*;

    fn test_config() -> Config {
        Config::from_tables(
            [
                "deps",
                "srcs",
                "visibility",
                "external_deps",
                "exported_external_deps",
            ]
            .into_iter()
            .map(String::from)
            .collect(),
            ["genrule.srcs"].into_iter().map(String::from).collect(),
            Default::default(),
        )
    }

    fn run(source: &str) -> String {
        run_with_config(source, &test_config())
    }

    fn run_with_config(source: &str, config: &Config) -> String {
        ParsedModule::parse(Cow::Borrowed(source))
            .and_then(|module| module.run_transform(|m| collect_edits(m, config, true)))
            .expect("failed")
            .unparse()
    }

    #[test]
    fn test_sort_deps() {
        let source = "my_rule(name=\"test\", deps=[\":z\", \":a\", \":m\"])\n";
        assert_eq!(
            run(source),
            "my_rule(name=\"test\", deps=[\":a\", \":m\", \":z\"])\n"
        );
    }

    #[test]
    fn test_sort_srcs() {
        let source = "rust_library(name=\"lib\", srcs=[\"z.rs\", \"a.rs\", \"m.rs\"])\n";
        assert_eq!(
            run(source),
            "rust_library(name=\"lib\", srcs=[\"a.rs\", \"m.rs\", \"z.rs\"])\n"
        );
    }

    #[test]
    fn test_genrule_srcs_not_sorted() {
        // genrule.srcs is blocklisted
        let source = "genrule(name=\"gen\", srcs=[\"z.txt\", \"a.txt\"])\n";
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_genrule_deps_still_sorted() {
        // Only srcs is blocklisted for genrule, not deps
        let source = "genrule(name=\"gen\", deps=[\":z\", \":a\"])\n";
        assert_eq!(
            run(source),
            "genrule(name=\"gen\", deps=[\":a\", \":z\"])\n"
        );
    }

    #[test]
    fn test_non_allowlisted_arg_not_sorted() {
        let source = "my_rule(name=\"test\", custom_list=[\"z\", \"a\"])\n";
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_non_string_elements_preserved() {
        let source = "my_rule(deps=[\":z\", some_var, \":a\"])\n";
        // some_var stays in place, strings around it are NOT sorted because
        // we can only sort contiguous runs, and there's only one string on each side
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_already_sorted() {
        let source = "my_rule(deps=[\":a\", \":b\", \":c\"])\n";
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_deduplicates_while_sorting() {
        let source = "my_rule(deps=[\":b\", \":a\", \":a\", \":b\"])\n";
        assert_eq!(run(source), "my_rule(deps=[\":a\", \":b\"])\n");
    }

    #[test]
    fn test_buildifier_string_sort_phases() {
        let source = "my_rule(deps=[\"@repo//z:z\", \"//b:b\", \":a\", \"plain\"])\n";
        assert_eq!(
            run(source),
            "my_rule(deps=[\"plain\", \":a\", \"//b:b\", \"@repo//z:z\"])\n"
        );
    }

    #[test]
    fn test_empty_list() {
        let source = "my_rule(deps=[])\n";
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_single_element_list() {
        let source = "my_rule(deps=[\":only\"])\n";
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_nested_calls() {
        // Inner call's deps list gets sorted
        let source = "outer(deps=[inner(deps=[\":z\", \":a\"])])\n";
        assert_eq!(run(source), "outer(deps=[inner(deps=[\":a\", \":z\"])])\n");
    }

    #[test]
    fn test_visibility() {
        let source = "my_rule(visibility=[\"//z:z\", \"//a:a\"])\n";
        assert_eq!(run(source), "my_rule(visibility=[\"//a:a\", \"//z:z\"])\n");
    }

    #[test]
    fn test_custom_config() {
        let config = Config::from_tables(
            ["my_custom_arg"].into_iter().map(String::from).collect(),
            HashSet::new(),
            Default::default(),
        );

        let source = "my_rule(my_custom_arg=[\"z\", \"a\"])\n";
        assert_eq!(
            run_with_config(source, &config),
            "my_rule(my_custom_arg=[\"a\", \"z\"])\n"
        );
    }

    #[test]
    fn test_relative_targets_sort_before_absolute() {
        let source = "my_rule(deps=[\"//lib/foo:foo\", \":bar\", \"//lib/baz:baz\"])\n";
        assert_eq!(
            run(source),
            "my_rule(deps=[\":bar\", \"//lib/baz:baz\", \"//lib/foo:foo\"])\n"
        );
    }

    #[test]
    fn test_relative_targets_idempotent() {
        let source = "my_rule(deps=[\":bar\", \"//lib/baz:baz\", \"//lib/foo:foo\"])\n";
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_colon_before_slash_in_paths() {
        let source = "my_rule(deps=[\"//tools/apple/plugins:plugin\", \"//tools/apple:lib\"])\n";
        assert_eq!(
            run(source),
            "my_rule(deps=[\"//tools/apple:lib\", \"//tools/apple/plugins:plugin\"])\n"
        );
    }

    // Tests for # keep sorted directive

    #[test]
    fn test_keep_sorted_leading_comment_before_assignment() {
        let source = "# keep sorted\nFOO = [\n    \"Z\",\n    \"A\",\n]\n";
        assert_eq!(
            run(source),
            "# keep sorted\nFOO = [\n    \"A\",\n    \"Z\",\n]\n"
        );
    }

    #[test]
    fn test_keep_sorted_leading_comment_on_first_element() {
        let source = "BAR = [\n    # keep sorted\n    \"Z\",\n    \"A\",\n]\n";
        assert_eq!(
            run(source),
            "BAR = [\n    # keep sorted\n    \"A\",\n    \"Z\",\n]\n"
        );
    }

    #[test]
    fn test_keep_sorted_inline_list() {
        let source = "# keep sorted\nFOO = [\"Z\", \"A\", \"M\"]\n";
        assert_eq!(run(source), "# keep sorted\nFOO = [\"A\", \"M\", \"Z\"]\n");
    }

    #[test]
    fn test_no_directive_no_sort() {
        // Without directive, standalone lists should NOT be sorted
        let source = "FOO = [\"Z\", \"A\"]\n";
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_keep_sorted_already_sorted() {
        let source = "# keep sorted\nFOO = [\"A\", \"B\", \"C\"]\n";
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_keep_sorted_empty_list() {
        let source = "# keep sorted\nFOO = []\n";
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_keep_sorted_single_element() {
        let source = "# keep sorted\nFOO = [\"only\"]\n";
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_keep_sorted_with_target_paths() {
        let source = "# keep sorted\nDEPS = [\"//lib/foo:foo\", \":bar\", \"//lib/baz:baz\"]\n";
        assert_eq!(
            run(source),
            "# keep sorted\nDEPS = [\":bar\", \"//lib/baz:baz\", \"//lib/foo:foo\"]\n"
        );
    }

    #[test]
    fn test_keep_sorted_multiple_statements() {
        let source = r#"# keep sorted
FOO = ["Z", "A"]

# not sorted
BAR = ["Z", "A"]

# keep sorted
BAZ = ["Z", "A"]
"#;
        let expected = r#"# keep sorted
FOO = ["A", "Z"]

# not sorted
BAR = ["Z", "A"]

# keep sorted
BAZ = ["A", "Z"]
"#;
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_keep_sorted_both_directive_and_function_arg() {
        // Both the directive-annotated list and the deps arg should be sorted
        let source = r#"# keep sorted
MY_LIST = [":z", ":a"]

my_rule(deps=[":z", ":a"])
"#;
        let expected = r#"# keep sorted
MY_LIST = [":a", ":z"]

my_rule(deps=[":a", ":z"])
"#;
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_keep_sorted_both_outer_and_inner_directive_idempotent() {
        // Regression: both `# keep sorted` before the assignment AND on the
        // first element used to schedule two sorts on the same list. With
        // dedup-induced length changes, the second `replace_range` apply
        // would use stale offsets and corrupt output. `handled_lists`
        // short-circuits the second call.
        let source = indoc! {r#"
            # keep sorted
            FOO = [
                # keep sorted
                "z",
                "a",
            ]
        "#};
        let expected = indoc! {r#"
            # keep sorted
            FOO = [
                # keep sorted
                "a",
                "z",
            ]
        "#};
        assert_eq!(run(source), expected);
        // Idempotent: formatting twice produces the same output.
        assert_eq!(run(&run(source)), expected);
    }

    // Tests for external_deps tuple sorting

    #[test]
    fn test_external_deps_all_tuples_sorted() {
        let source = indoc! {r#"
            cpp_library(
                name = "x",
                external_deps = [
                    ("boost", None, "boost_uuid"),
                    ("boost", None, "boost_filesystem"),
                    ("glibc", None, "dl"),
                ],
            )
        "#};
        let expected = indoc! {r#"
            cpp_library(
                name = "x",
                external_deps = [
                    ("boost", None, "boost_filesystem"),
                    ("boost", None, "boost_uuid"),
                    ("glibc", None, "dl"),
                ],
            )
        "#};
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_external_deps_mixed_string_and_tuple_sorted() {
        // A bare string's key is just the project (e.g. "boost"), which the
        // split-on-`:` comparator orders before a same-project tuple key like
        // "boost:boost_filesystem:" — so the bare string sorts first.
        let source = indoc! {r#"
            cpp_library(
                name = "x",
                exported_external_deps = [
                    ("boost", None, "boost_filesystem"),
                    "boost",
                    ("boost", None, "boost_uuid"),
                ],
            )
        "#};
        let expected = indoc! {r#"
            cpp_library(
                name = "x",
                exported_external_deps = [
                    "boost",
                    ("boost", None, "boost_filesystem"),
                    ("boost", None, "boost_uuid"),
                ],
            )
        "#};
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_external_deps_single_line_tuples_sorted() {
        let source = "cpp_library(external_deps=[(\"glibc\", None, \"dl\"), (\"boost\", None, \"boost_uuid\")])\n";
        assert_eq!(
            run(source),
            "cpp_library(external_deps=[(\"boost\", None, \"boost_uuid\"), (\"glibc\", None, \"dl\")])\n"
        );
    }

    #[test]
    fn test_external_deps_already_sorted_no_change() {
        let source = indoc! {r#"
            cpp_library(
                external_deps = [
                    ("boost", None, "boost_filesystem"),
                    ("glibc", None, "dl"),
                ],
            )
        "#};
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_external_deps_tuple_dedup() {
        let source = indoc! {r#"
            cpp_library(
                external_deps = [
                    ("glibc", None, "dl"),
                    ("boost", None, "boost_uuid"),
                    ("glibc", None, "dl"),
                ],
            )
        "#};
        let expected = indoc! {r#"
            cpp_library(
                external_deps = [
                    ("boost", None, "boost_uuid"),
                    ("glibc", None, "dl"),
                ],
            )
        "#};
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_external_deps_non_string_tuple_element_is_barrier() {
        // A tuple containing a non-string/None element (here a bool) is not
        // sortable and stays in place, breaking the surrounding run.
        let source = indoc! {r#"
            cpp_library(
                external_deps = [
                    ("z", None, "z"),
                    ("mid", True),
                    ("a", None, "a"),
                ],
            )
        "#};
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_external_deps_bare_strings_sorted() {
        let source = "cpp_library(external_deps=[\"gflags\", \"boost\"])\n";
        assert_eq!(
            run(source),
            "cpp_library(external_deps=[\"boost\", \"gflags\"])\n"
        );
    }

    #[test]
    fn test_external_deps_over_length_tuple_is_barrier() {
        // A 4+-element tuple isn't a valid external_deps entry; since the key
        // only encodes the first three positions, it must not be sorted (which
        // could silently dedup distinct entries). It stays in place.
        let source = indoc! {r#"
            cpp_library(
                external_deps = [
                    ("z", None, "z"),
                    ("boost", None, "x", "extra"),
                    ("a", None, "a"),
                ],
            )
        "#};
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_external_deps_version_tiebreak() {
        // Same project and name, differing non-None versions: the version
        // component (last in the key) breaks the tie, ordered lexicographically.
        let source = indoc! {r#"
            cpp_library(
                external_deps = [
                    ("openssl", "1.1.0", "ssl"),
                    ("openssl", "1.0.2", "ssl"),
                ],
            )
        "#};
        let expected = indoc! {r#"
            cpp_library(
                external_deps = [
                    ("openssl", "1.0.2", "ssl"),
                    ("openssl", "1.1.0", "ssl"),
                ],
            )
        "#};
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_external_deps_short_tuple_sorted() {
        // A 2-element tuple (name position absent) is still sortable — the
        // missing position keys as an empty component, not a barrier.
        let source = indoc! {r#"
            cpp_library(
                external_deps = [
                    ("gflags", None),
                    ("boost", None, "boost_uuid"),
                    ("boost", None),
                ],
            )
        "#};
        let expected = indoc! {r#"
            cpp_library(
                external_deps = [
                    ("boost", None),
                    ("boost", None, "boost_uuid"),
                    ("gflags", None),
                ],
            )
        "#};
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_external_deps_empty_string_component_is_barrier() {
        // An explicit empty-string component keys identically to a `None`/missing
        // position, so it's left in place rather than risk a silent dedup against
        // a `None`-valued tuple. Critically, neither entry is dropped.
        let source = indoc! {r#"
            cpp_library(
                external_deps = [
                    ("boost", None, "x"),
                    ("boost", "", "x"),
                ],
            )
        "#};
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_external_deps_colon_in_component_is_barrier() {
        // A `:` inside a component makes the flattened key ambiguous, so such
        // tuples are left in place — critically, neither is dropped as a false
        // duplicate (both share the naive key `a:b:c:`).
        let source = indoc! {r#"
            cpp_library(
                external_deps = [
                    ("a", None, "b:c"),
                    ("a:b", None, "c"),
                ],
            )
        "#};
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_external_deps_idempotent() {
        let source = indoc! {r#"
            cpp_library(
                external_deps = [
                    ("boost", None, "boost_uuid"),
                    "boost",
                    ("boost", None, "boost_filesystem"),
                ],
            )
        "#};
        let expected = indoc! {r#"
            cpp_library(
                external_deps = [
                    "boost",
                    ("boost", None, "boost_filesystem"),
                    ("boost", None, "boost_uuid"),
                ],
            )
        "#};
        // Pin the concrete output (so a wrong-but-stable ordering fails here),
        // then confirm a second pass is a no-op.
        let once = run(source);
        assert_eq!(once, expected);
        assert_eq!(run(&once), once);
    }

    #[test]
    fn test_keep_sorted_external_deps_tuples_force_sorted() {
        // `# keep sorted` force-sorts a standalone list made of tuples.
        let source = indoc! {r#"
            # keep sorted
            DEPS = [
                ("glibc", None, "dl"),
                ("boost", None, "boost_uuid"),
            ]
        "#};
        let expected = indoc! {r#"
            # keep sorted
            DEPS = [
                ("boost", None, "boost_uuid"),
                ("glibc", None, "dl"),
            ]
        "#};
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_keep_sorted_mixed_list_not_force_sorted() {
        let source = indoc! {r#"
            # keep sorted
            MIXED = [
                ":z",
                (":tuple", True),
                ":a",
            ]
        "#};
        assert_eq!(run(source), source);
    }

    // Comprehensive tests for comment handling

    #[test]
    fn test_leading_comments_are_group_separators() {
        // Each leading comment creates a group boundary.
        // Single-element groups can't be reordered.
        let source = r#"my_rule(deps=[
    # Z library
    ":z",
    # A library
    ":a",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_three_groups_of_one_element() {
        // Three comments = three groups of one element each. No reordering.
        let source = r#"my_rule(deps=[
    # Third
    ":z",
    # First
    ":a",
    # Second
    ":m",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_multiline_comments_are_group_separators() {
        // Multi-line comments still act as group separators.
        let source = r#"my_rule(deps=[
    # This is a longer comment
    # that spans multiple lines
    ":z",
    # Short comment
    ":a",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_comment_starts_group_elements_sort_within() {
        // Buildifier does not sort non-forced lists with line comments.
        let source = r#"my_rule(deps=[
    # Has comment
    ":z",
    ":a",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_already_sorted_with_comments() {
        // Already sorted - should remain unchanged
        let source = r#"my_rule(deps=[
    # A comment
    ":a",
    # Z comment
    ":z",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_keep_sorted_directive_with_leading_comments() {
        // Each element has a leading comment → each is its own group. No reordering.
        let source = r#"# keep sorted
DEPS = [
    # Z dep
    ":z",
    # A dep
    ":a",
]
"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_comments_with_target_path_sorting() {
        // Each element has a leading comment → separate groups. No reordering.
        let source = r#"my_rule(deps=[
    # External dep
    "//lib/z:z",
    # Relative dep
    ":a",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_empty_lines_between_elements() {
        // Empty lines between elements should be preserved during sort
        let source = r#"my_rule(deps=[

    ":z",

    ":a",
])"#;
        let expected = r#"my_rule(deps=[

    ":a",

    ":z",
])"#;
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_comments_and_blank_lines_mixed() {
        // Comments with blank lines still act as group separators. No reordering.
        let source = r#"my_rule(deps=[

    # Z comment
    ":z",

    # A comment
    ":a",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_buildifier_keep_sorted_stays_pinned() {
        // `# buildifier: keep sorted` is excluded from comment detection.
        // `# Various helpers` and `# A dep` are group separators.
        // Each group has 1 element, so no reordering.
        let source = r#"my_rule(deps=[
    # buildifier: keep sorted
    # Various helpers
    "//z:z",
    # A dep
    "//a:a",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_keep_sorted_inside_list_stays_pinned() {
        // `# keep sorted` inside the list stays pinned at top
        let source = r#"my_rule(deps=[
    # keep sorted
    ":z",
    ":a",
])"#;
        let expected = r#"my_rule(deps=[
    # keep sorted
    ":a",
    ":z",
])"#;
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_multiple_elements_per_group_sorted_independently() {
        // Buildifier does not sort non-forced lists with line comments.
        let source = r#"my_rule(deps=[
    # Group A
    ":z",
    ":a",
    ":m",
    # Group B
    ":y",
    ":b",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_section_headers_wa_msys_style() {
        // Simulates wa-msys/BUCK pattern: section header comments with deps underneath
        let source = r#"my_rule(deps=[
    # buildifier: keep sorted
    # Various helpers
    "//z:helper_z",
    "//a:helper_a",
    # Connection plugins
    "//z:conn_z",
    "//a:conn_a",
    "//m:conn_m",
])"#;
        let expected = r#"my_rule(deps=[
    # buildifier: keep sorted
    # Various helpers
    "//a:helper_a",
    "//z:helper_z",
    # Connection plugins
    "//a:conn_a",
    "//m:conn_m",
    "//z:conn_z",
])"#;
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_already_sorted_groups_no_change() {
        // Groups are already sorted internally — no edits needed.
        let source = r#"my_rule(deps=[
    # Group 1
    ":a",
    ":z",
    # Group 2
    ":b",
    ":y",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_single_element_groups_no_change() {
        // Every element has its own comment header → no sorting possible
        let source = r#"my_rule(deps=[
    # Header Z
    ":z",
    # Header A
    ":a",
    # Header M
    ":m",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_mixed_group_sizes() {
        // Buildifier does not sort non-forced lists with line comments.
        let source = r#"my_rule(deps=[
    # Big group
    ":z",
    ":a",
    ":m",
    # Singleton
    ":only",
    # Pair
    ":y",
    ":b",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_trailing_inline_comments_move_with_elements() {
        // Trailing inline comments should move with their element, not drift
        let source = r#"my_rule(deps=[
    ":z",  # z dep
    ":a",  # a dep
])"#;
        let expected = r#"my_rule(deps=[
    ":a",  # a dep
    ":z",  # z dep
])"#;
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_trailing_inline_within_group() {
        // The leading line comment prevents sorting in Buildifier-compatible mode.
        let source = r#"my_rule(deps=[
    # Group A
    ":z",  # z inline
    ":a",  # a inline
    # Group B
    ":y",
])"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_line_comments_deduplicate_without_sorting() {
        let source = r#"my_rule(deps=[
    # Commented group
    ":z",
    ":a",
    ":z",
])"#;
        let expected = r#"my_rule(deps=[
    # Commented group
    ":z",
    ":a",
])"#;
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_three_elements_with_trailing_comments() {
        let source = r#"my_rule(deps=[
    ":z",  # third
    ":a",  # first
    ":m",  # second
])"#;
        let expected = r#"my_rule(deps=[
    ":a",  # first
    ":m",  # second
    ":z",  # third
])"#;
        assert_eq!(run(source), expected);
    }

    // Tests for BinOp list sorting (deps = [...] + [...])

    #[test]
    fn test_binop_list_concat_sorted() {
        // Lists in BinOp (+ concat) should be sorted individually
        let source = r#"my_rule(
    deps = [
        ":z",
        ":a",
    ] + [
        ":y",
        ":b",
    ],
)"#;
        let expected = r#"my_rule(
    deps = [
        ":a",
        ":z",
    ] + [
        ":b",
        ":y",
    ],
)"#;
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_binop_with_select() {
        // Lists before/after select() should be sorted; lists inside select() too
        let source = r#"my_rule(
    deps = [
        ":z",
        ":a",
    ] + select({
        "DEFAULT": [":z_default", ":a_default"],
    }),
)"#;
        let expected = r#"my_rule(
    deps = [
        ":a",
        ":z",
    ] + select({
        "DEFAULT": [":a_default", ":z_default"],
    }),
)"#;
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_select_only() {
        // select() as direct kwarg value: sort list values inside
        let source = r#"my_rule(
    deps = select({
        "config//os:linux": [":z_linux", ":a_linux"],
        "config//os:macos": [":z_macos", ":a_macos"],
        "DEFAULT": [],
    }),
)"#;
        let expected = r#"my_rule(
    deps = select({
        "config//os:linux": [":a_linux", ":z_linux"],
        "config//os:macos": [":a_macos", ":z_macos"],
        "DEFAULT": [],
    }),
)"#;
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_binop_already_sorted() {
        let source = r#"my_rule(
    deps = [
        ":a",
        ":z",
    ] + [
        ":b",
    ],
)"#;
        assert_eq!(run(source), source);
    }

    #[test]
    fn test_non_sortable_arg_binop_not_sorted() {
        // non-sortable args should not be sorted even in BinOp form
        let source = r#"my_rule(
    custom = [
        ":z",
        ":a",
    ] + [
        ":y",
    ],
)"#;
        assert_eq!(run(source), source);
    }

    // Tests for trailing comma insertion

    #[test]
    fn test_last_element_no_trailing_comma() {
        // Last element without comma moves to non-last position: comma must be added
        // to prevent implicit string concatenation. The element moving to last position
        // keeps its original comma (trailing commas are valid in Starlark).
        let source = indoc! {r#"
            my_rule(deps=[
                ":z",
                ":m",
                ":a"
            ])
        "#};
        let expected = indoc! {r#"
            my_rule(deps=[
                ":a",
                ":m",
                ":z",
            ])
        "#};
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_last_element_no_comma_with_comment() {
        // Last element without comma but with trailing comment
        // Comma is inserted after the element value, before the comment
        let source = indoc! {r#"
            my_rule(deps=[
                ":z",
                ":a"  # no comma
            ])
        "#};
        let expected = indoc! {r#"
            my_rule(deps=[
                ":a",  # no comma
                ":z",
            ])
        "#};
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_last_element_no_comma_three_elements() {
        // Three elements, last without comma
        let source = indoc! {r#"
            my_rule(deps=[
                ":z",
                ":a",
                ":m"
            ])
        "#};
        let expected = indoc! {r#"
            my_rule(deps=[
                ":a",
                ":m",
                ":z",
            ])
        "#};
        assert_eq!(run(source), expected);
    }

    #[test]
    fn test_multiline_list_with_same_line_elements_does_not_panic() {
        let source = indoc! {r#"
            my_rule(
                deps = [
                    ":b", ":a",
                    ":c",
                ],
            )
        "#};
        let expected = indoc! {r#"
            my_rule(
                deps = [
                    ":a", ":b", ":c",
                ],
            )
        "#};
        assert_eq!(run(source), expected);
    }
}
