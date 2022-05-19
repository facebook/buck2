RuleRegistrationSpec = record(
    name = field(str.type),
    implementation = field("function"),
    attributes = field({str.type: "attribute"}),
    cfg = field([None, "transition"], None),
)
