FullyDocumentedInfo = provider(
    doc = "Summary for FullyDocumentedInfo\n\nDetails for FullyDocumentedInfo",
    fields = {"a": "", "b": "b summary", "c": "c summary\n\nAnd c details"},
)

SummaryInfo = provider(
    doc = "Summary for SummaryInfo",
    fields = {"a": "", "b": "b summary", "c": "c summary\n\nAnd c details"},
)

NoDocstringInfo = provider(
    fields = {"a": "", "b": "b summary", "c": "c summary\n\nAnd c details"},
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
