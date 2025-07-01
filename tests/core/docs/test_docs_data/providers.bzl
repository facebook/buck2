# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

SummaryInfo = provider(
    doc = "Summary for SummaryInfo",
    fields = ["a", "b", "c"],
)

NoDocstringInfo = provider(
    fields = ["a", "b", "c"],
)

SimpleDocumentedInfo = provider(
    doc = "Summary for SimpleDocumentedInfo\n\nDetails for SimpleDocumentedInfo",
    fields = ["a", "b", "c"],
)

SimpleSummaryInfo = provider(
    doc = "Summary for SimpleSummaryInfo",
    fields = ["a", "b", "c"],
)

SimpleNoDocstringInfo = provider(
    fields = ["a", "b", "c"],
)
