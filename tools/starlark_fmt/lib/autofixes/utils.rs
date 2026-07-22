/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Shared utilities for Starlark autofixes.

use std::borrow::Cow;

use ruff_python_trivia::SimpleTokenKind;
use ruff_python_trivia::SimpleTokenizer;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

use super::parsed_module::ParsedModule;

pub(crate) fn find_ignore_ascii_case(haystack: &str, needle: &str) -> Option<usize> {
    if needle.is_empty() {
        return Some(0);
    }

    let needle_bytes = needle.as_bytes();
    haystack
        .as_bytes()
        .windows(needle.len())
        .position(|window| window.eq_ignore_ascii_case(needle_bytes))
}

/// Check if the file contains a comment with the given needle (case-insensitive).
pub(crate) fn file_has_comment(module: &ParsedModule, needle: &str) -> bool {
    let range = TextRange::new(TextSize::from(0), TextSize::of(module.source()));
    module
        .comments_in_range(range)
        .any(|comment| find_ignore_ascii_case(comment, needle).is_some())
}

/// Find the start of leading comments for a statement at the given line start.
///
/// Scans backwards from `line_start` to find consecutive comment lines. Returns
/// the byte offset where the leading comment block starts. If there are no
/// leading comments, returns `line_start`.
pub(crate) fn find_leading_comment_start(source: &str, line_start: usize) -> usize {
    if line_start == 0 {
        return 0;
    }

    let mut result = line_start;
    let mut pos = line_start;

    // Scan backwards line by line
    while pos > 0 {
        // Find the start of the previous line
        let prev_newline = source[..pos.saturating_sub(1)].rfind('\n');
        let prev_line_start = prev_newline.map(|p| p + 1).unwrap_or(0);
        let prev_line = &source[prev_line_start..pos.saturating_sub(1)];
        let trimmed = prev_line.trim();

        if trimmed.starts_with('#') {
            // This is a comment line - include it
            result = prev_line_start;
            pos = prev_line_start;
        } else {
            // Non-comment line - stop scanning
            break;
        }
    }

    result
}

/// Find the end of the line containing the given byte offset.
///
/// Returns the offset of the newline character, or end of source if no
/// newline is found. This is used by both kwarg and list sorting to compute
/// line-based block ranges for comment-preserving reordering.
pub(crate) fn find_line_end(source: &str, offset: TextSize) -> TextSize {
    let start = offset.to_usize();
    let rest = &source[start..];
    match rest.find('\n') {
        Some(pos) => TextSize::from((start + pos) as u32),
        None => TextSize::of(source),
    }
}

/// Advance past a newline character at the given offset, if present.
///
/// Returns `offset + 1` if the byte at `offset` is `\n`, otherwise returns
/// `offset` unchanged. Useful after `find_line_end` to get the start of the
/// next line.
pub(crate) fn offset_past_newline(source: &str, offset: TextSize) -> TextSize {
    if offset.to_usize() < source.len() && source.as_bytes().get(offset.to_usize()) == Some(&b'\n')
    {
        offset + TextSize::from(1)
    } else {
        offset
    }
}

/// Find the start of the line containing the given byte offset.
///
/// Scans backwards from `offset` to find the preceding newline, returning
/// the position immediately after it. Returns 0 if no newline is found
/// (i.e., the offset is on the first line).
pub(crate) fn line_start(source: &str, offset: usize) -> usize {
    source[..offset].rfind('\n').map_or(0, |pos| pos + 1)
}

/// Compute line-based block ranges for a sequence of elements.
///
/// Each element's "block" spans from the line after the previous element's end
/// to the end of the current element's line, capturing leading comments and
/// trailing inline comments. The first element's block starts at `first_block_start`.
pub(crate) fn compute_block_ranges(
    source: &str,
    first_block_start: TextSize,
    element_ends: &[TextSize],
) -> Vec<TextRange> {
    element_ends
        .iter()
        .enumerate()
        .map(|(idx, &elt_end)| {
            let block_start = if idx == 0 {
                first_block_start
            } else {
                let prev_end = find_line_end(source, element_ends[idx - 1]);
                offset_past_newline(source, prev_end)
            };

            let elt_line_end = find_line_end(source, elt_end);
            let block_end = offset_past_newline(source, elt_line_end);

            TextRange::new(block_start, block_end)
        })
        .collect()
}

/// Validate that all block ranges fall within the source text bounds.
pub(crate) fn block_ranges_valid(ranges: &[TextRange], source: &str) -> bool {
    let source_len = TextSize::of(source);
    !ranges
        .iter()
        .any(|r| r.start() > source_len || r.end() > source_len)
}

/// Return the source text of `block_range` with a comma guaranteed after the
/// element/value whose source ends at `elt_end`.
///
/// Lists, dicts, and call argument lists may legally omit the trailing comma on
/// their final element. Every sorting pass that reorders such elements must run
/// this before placing a block ahead of a sibling — otherwise the comma-less
/// element splices into the next one (e.g. `"a": None` directly followed by
/// `"z": 1`), producing source that fails to re-parse.
///
/// Whether a comma already follows the element is decided with ruff's
/// `SimpleTokenizer` — the same probe ruff's own formatter uses for magic
/// trailing commas — which skips whitespace, comments, newlines, and closing
/// parens. That avoids the false positives a raw byte scan invites (a comma
/// inside a string or comment, or one sitting past a line break).
///
/// The result borrows from `source` unchanged whenever a comma is already
/// present, so a fresh `String` is allocated only when one must be inserted.
/// Returns `None` when `block_range`/`elt_end` are inconsistent (the caller
/// should fall back to a safer strategy); the offsets come from AST node ranges,
/// so in practice this only guards against misuse.
pub(crate) fn block_with_trailing_comma(
    source: &str,
    block_range: TextRange,
    elt_end: TextSize,
) -> Option<Cow<'_, str>> {
    let block = source.get(block_range.start().to_usize()..block_range.end().to_usize())?;
    let elt_end_in_block = elt_end
        .to_usize()
        .checked_sub(block_range.start().to_usize())?;
    if elt_end_in_block > block.len() {
        return None;
    }
    if element_is_comma_terminated(source, elt_end, block_range.end()) {
        return Some(Cow::Borrowed(block));
    }
    let mut with_comma = String::with_capacity(block.len() + 1);
    with_comma.push_str(&block[..elt_end_in_block]);
    with_comma.push(',');
    with_comma.push_str(&block[elt_end_in_block..]);
    Some(Cow::Owned(with_comma))
}

/// True if the next significant token after `elt_end`, scanning only up to
/// `block_end`, is a comma. Mirrors ruff's `has_trailing_comma`: closing parens
/// are skipped (an element's AST range can end before the parens that wrap it),
/// and trivia — whitespace, comments, newlines — never count.
///
/// The scan is bounded to the element's own block so it cannot escape into an
/// enclosing construct: for the final element of a nested collection there is no
/// trailing comma, and an unbounded scan would skip the closing `)` and latch
/// onto an outer separator, wrongly reporting the element as comma-terminated.
fn element_is_comma_terminated(source: &str, elt_end: TextSize, block_end: TextSize) -> bool {
    SimpleTokenizer::new(source, TextRange::new(elt_end, block_end))
        .skip_trivia()
        .find(|token| token.kind() != SimpleTokenKind::RParen)
        .is_some_and(|token| token.kind() == SimpleTokenKind::Comma)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Run `block_with_trailing_comma` over the whole of `source` as the block,
    /// with the element ending at byte offset `elt_end`.
    fn comma(source: &str, elt_end: u32) -> Option<Cow<'_, str>> {
        block_with_trailing_comma(
            source,
            TextRange::new(TextSize::from(0), TextSize::of(source)),
            TextSize::from(elt_end),
        )
    }

    #[test]
    fn test_inserts_comma_when_missing() {
        // `    "a": None\n` — value ends after `None`, 13 bytes in.
        let result = comma("    \"a\": None\n", 13).expect("offset is in-bounds");
        assert!(matches!(result, Cow::Owned(_)), "must allocate to insert");
        assert_eq!(result, "    \"a\": None,\n");
    }

    #[test]
    fn test_borrows_unchanged_when_comma_present() {
        let result = comma("    \"a\": None,\n", 13).expect("offset is in-bounds");
        assert!(
            matches!(result, Cow::Borrowed(_)),
            "must not allocate when a comma is already present"
        );
        assert_eq!(result, "    \"a\": None,\n");
    }

    #[test]
    fn test_inserts_comma_before_trailing_inline_comment() {
        // The comment is trivia; with no real comma the helper inserts one right
        // after the value, before the comment.
        let result = comma("    \"a\": None  # note\n", 13).expect("offset is in-bounds");
        assert_eq!(result, "    \"a\": None,  # note\n");
    }

    #[test]
    fn test_comma_inside_comment_is_not_mistaken_for_a_trailing_comma() {
        // A byte scan that merely looks for `,` could be fooled by the comma
        // inside the comment; the tokenizer treats the whole comment as trivia.
        let result = comma("    \"a\": None  # a, b\n", 13).expect("offset is in-bounds");
        assert_eq!(result, "    \"a\": None,  # a, b\n");
    }

    #[test]
    fn test_detects_comma_past_a_newline() {
        // The comma sits on the next line. A scan that stopped at the first
        // newline would wrongly insert a second comma; the tokenizer finds it.
        let result = comma("    \"a\": None\n    ,\n", 13).expect("offset is in-bounds");
        assert!(
            matches!(result, Cow::Borrowed(_)),
            "a comma after a line break is still a trailing comma"
        );
    }

    #[test]
    fn test_honors_nonzero_block_range_start() {
        // The block is a sub-slice that does not start at offset 0.
        let source = "prefix =\n\"z\": 1\n";
        let block_range = TextRange::new(TextSize::from(9), TextSize::of(source));
        // `"z": 1` value ends after `1`, at offset 15.
        let result = block_with_trailing_comma(source, block_range, TextSize::from(15))
            .expect("offset is in-bounds");
        assert_eq!(result, "\"z\": 1,\n");
    }

    #[test]
    fn test_element_end_exactly_at_block_end() {
        // No trailing newline: the value ends at the block's final byte. The
        // comma is appended at the very end.
        let result = comma("    \"a\": None", 13).expect("offset is in-bounds");
        assert_eq!(result, "    \"a\": None,");
    }

    #[test]
    fn test_multibyte_content_before_element_end_does_not_panic() {
        // A non-ASCII char sits inside the block before `elt_end`. Slicing must
        // land on a char boundary (AST offsets always do), never mid-codepoint.
        // `"café"` is 7 bytes (é is 2), so the value ends at byte offset 7.
        let result = comma("\"café\"\n", 7).expect("offset is in-bounds");
        assert_eq!(result, "\"café\",\n");
    }

    #[test]
    fn test_returns_none_when_offset_out_of_block() {
        assert!(
            comma("\"z\": 1\n", 999).is_none(),
            "out-of-bounds element end signals the caller to fall back"
        );
    }
}
