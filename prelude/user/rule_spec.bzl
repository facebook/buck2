RuleRegistrationSpec = record(
    name = field(str.type),
    impl = field("function"),
    attributes = field({str.type: "attribute"}),
    cfg = field([None, "transition"], None),
)
