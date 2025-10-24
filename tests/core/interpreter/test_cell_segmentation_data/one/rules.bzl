Info = provider(
    fields = {
        "label": provider_field(Label),
    },
)

def project_as_json(value: Info):
    if value == None:
        return struct()
    return struct(label = str(value.label))

InfoTSet = transitive_set(json_projections = {"json": project_as_json})

InfoGraph = provider(
    fields = {
        "tset": provider_field(InfoTSet),
    },
)

library = rule(
    impl = lambda ctx: [
        DefaultInfo(default_outputs = []),
        InfoGraph(tset = ctx.actions.tset(
            InfoTSet,
            value = Info(label = ctx.label),
            children = [
                dep[InfoGraph].tset
                for dep in ctx.attrs.deps
            ],
        )),
    ],
    attrs = {
        "deps": attrs.list(attrs.dep(providers = [InfoGraph]), default = []),
    },
)

def __binary(ctx: AnalysisContext) -> list[Provider]:
    tset = ctx.actions.tset(
        InfoTSet,
        children = [
            dep[InfoGraph].tset
            for dep in ctx.attrs.deps
        ],
    )
    json = tset.project_as_json("json")
    json = ctx.actions.write_json("out.json", json)
    return [DefaultInfo(default_output = json)]

binary = rule(
    impl = __binary,
    attrs = {
        "deps": attrs.list(attrs.dep(providers = [InfoGraph]), default = []),
    },
)
