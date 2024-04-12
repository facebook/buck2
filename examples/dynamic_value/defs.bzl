load("@prelude//utils:graph_utils.bzl", "post_order_traversal")

def _module_paths(value: Artifact) -> cmd_args:
    return cmd_args(value, delimiter = "\n")

ModuleTSet = transitive_set(
    args_projections = {
        "paths": _module_paths,
    },
    json_projections = {
    },
)

DynamicModuleInfo = provider(fields = {
    "transitive_outputs": provider_field(ModuleTSet),
})

DynamicPackageInfo = provider(fields = {
    "modules": provider_field(dict[str, DynamicModuleInfo]),
})

ModuleInfo = provider(fields = {
    "source": provider_field(Artifact),
    "output": provider_field(Artifact),
})

PackageInfo = provider(fields = {
    "modules": provider_field(dict[str, ModuleInfo]),
    "dynamic": provider_field(DynamicValue),
})

def _dynamic_value_impl(ctx: AnalysisContext) -> list[Provider]:
    modules = {}
    for src in ctx.attrs.srcs:
        name = src.basename[:-len(src.extension)]
        output = ctx.actions.declare_output(name + ".out")
        modules[name] = ModuleInfo(
            source = src,
            output = output,
        )

    def dynamic_action(ctx, artifacts, resolved, outputs, modules=modules, deps=ctx.attrs.deps):
        graph = {}
        for name, module in modules.items():
            json = artifacts[module.source].read_json()
            graph[name] = json["deps"]

        tsets = {}
        externals = {}
        for dep in deps:
            pkg_info = dep[PackageInfo]
            dyn_info = resolved[pkg_info.dynamic][DynamicPackageInfo]
            for name in pkg_info.modules.keys():
                graph[name] = []
                tsets[name] = dyn_info.modules[name].transitive_outputs
                externals[name] = None

        for name in post_order_traversal(graph):
            if name in externals:
                continue

            inputs = ctx.actions.tset(
                ModuleTSet,
                children = [tsets[dep] for dep in graph[name]],
            )
            print(ctx.label.name, name, "INPUTS", list(inputs.traverse()))
            ctx.actions.write(outputs[modules[name].output], inputs.project_as_args("paths"))
            tsets[name] = ctx.actions.tset(
                ModuleTSet,
                value = modules[name].output,
                children = [inputs],
            )

        return [DynamicPackageInfo(
            modules = {
                name: DynamicModuleInfo(
                    transitive_outputs = tsets[name],
                )
                for name in modules.keys()
            },
        )]


    print([module.output for module in modules.values()])
    dynamic_value = ctx.actions.dynamic_output(
        dynamic = ctx.attrs.srcs,
        inputs = [],
        promises = [
            dep[PackageInfo].dynamic
            for dep in ctx.attrs.deps
        ],
        outputs = [module.output.as_output() for module in modules.values()],
        f = dynamic_action,
    )

    print("dynamic_value", dynamic_value)

    default_info = DefaultInfo(
        default_outputs = [module.output for module in modules.values()],
    )

    package_info = PackageInfo(
        modules = modules,
        dynamic = dynamic_value,
    )

    return [default_info, package_info]

dynamic_value = rule(
    impl = _dynamic_value_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
        "deps": attrs.list(attrs.dep(providers = [PackageInfo]), default = []),
    },
)
