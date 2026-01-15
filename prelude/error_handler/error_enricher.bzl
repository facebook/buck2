# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
Unified error enrichment framework for Buck2 structured build errors.

This module provides a shared abstraction for matching and enriching build errors
across different languages.
"""

load("@prelude//error_handler:error_enricher_types.bzl", "ErrorEnricher")

def match_error(matcher: str | BuckRegex, text: str) -> bool:
    if isinstance(matcher, str):
        return matcher in text
    elif isinstance(matcher, BuckRegex):
        return matcher.match(text)
    else:
        fail("Unknown matcher type: {}".format(type(matcher)))

def enrich_errors(
        errors: list[ActionSubError],
        enrichers: list[ErrorEnricher],
        category_prefix: str = "") -> list[ActionSubError]:
    """
    Add additional information to build errors based on a list of enrichers.
    """
    for error in errors:
        message = error.message if error.message else ""

        for enricher in enrichers:
            if not match_error(enricher.matcher, message):
                continue

            error.category = category_prefix + enricher.category

            # Subcategory extraction provides finer-grained classification
            # within a broad error category, enabling targeted remediations
            if enricher.subcategory_extractor:
                subcategory = enricher.subcategory_extractor(error.message)
                if subcategory:
                    error.subcategory = subcategory
                    if enricher.subcategory_remediations and subcategory in enricher.subcategory_remediations:
                        error.remediation = enricher.subcategory_remediations[subcategory]
                elif enricher.message:
                    error.remediation = enricher.message
            elif enricher.message:
                error.remediation = enricher.message
            break

    return errors
