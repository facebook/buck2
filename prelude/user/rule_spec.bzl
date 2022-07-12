RuleRegistrationSpec = record(
    name = field(str.type),
    impl = field("function"),
    attrs = field({str.type: "attribute"}),
    cfg = field([None, "transition"], None),
)
