load(
    "@fbcode//buck2/prelude/cxx:link.bzl",
    "cxx_link_into_shared_library",
    "cxx_link_shared_library",
)
load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "LinkArgs",
    "LinkInfo",
    "LinkStyle",
    "Linkage",
    "LinkedObject",
    "SharedLibLinkable",
    "get_actual_link_style",
    "link_info_to_args",
)
load(
    "@fbcode//buck2/prelude/linking:linkable_graph.bzl",
    "LinkableGraph",  # @unused Used as a type
    "LinkableNode",
    "get_deps_for_link",
    "get_link_info",
    "linkable_deps",
    "linkable_graph",
)
load(
    "@fbcode//buck2/prelude/utils:graph_utils.bzl",
    "breadth_first_traversal_by",
    "topo_sort",
)
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect", "flatten", "value_or")
load(":cxx_context.bzl", "get_cxx_toolchain_info")
load(
    ":linker.bzl",
    "get_default_shared_library_name",
    "get_ignore_undefined_symbols_flags",
    "get_no_as_needed_shared_libs_flags",
    "get_shared_library_name",
)
load(":symbols.bzl", "extract_symbol_names")

# A provider with information used to link a rule into a shared library.
# Potential omnibus roots must provide this so that omnibus can link them
# here, in the context of the top-level packaging rule.
NativeLinkTargetInfo = provider(fields = [
    "link_info",  # LinkInfo.type
    "name",  # [str.type, None]
    "deps",  # ["label"]
])

# The result of the omnibus link.
OmnibusSharedLibraries = record(
    libraries = field({str.type: LinkedObject.type}, {}),
    roots = field({"label": LinkedObject.type}, {}),
)

# Bookkeeping information used to setup omnibus link rules.
OmnibusSpec = record(
    body = field({"label": None}, {}),
    excluded = field({"label": None}, {}),
    roots = field({"label": (NativeLinkTargetInfo.type, "artifact")}, {}),
    # All link infos.
    link_infos = field({"label": LinkableNode.type}, {}),
)

def create_native_link_target(
        link_info: LinkInfo.type,
        name: [str.type, None] = None,
        deps: ["dependency"] = []) -> NativeLinkTargetInfo.type:
    return NativeLinkTargetInfo(
        name = name,
        link_info = link_info,
        # Only include dependencies that are linkable.
        deps = linkable_deps(deps),
    )

def add_omnibus_exclusions(
        graph: LinkableGraph.type,
        deps: ["dependency"]):
    """
    Mark all deps as excluded from omnibus linking.

    For example: Python libraries which wrap/package prebuilt native extensions
    must set this to prevent transitive native deps being merged, breaking the
    hard-coded `DT_NEEDED` tags in the extensions.
    """

    for dep in deps:
        dep_info = linkable_graph(dep)
        if dep_info != None:
            expect(dep_info.label in graph.nodes)
            graph.excluded[dep_info.label] = None

def add_omnibus_roots(
        graph: LinkableGraph.type,
        first_order_deps: ["dependency"] = []):
    """
    Non-native langue rules should call this to add roots nodes for all
    first-order native dependencies.
    """

    for dep in first_order_deps:
        if dep[NativeLinkTargetInfo]:
            graph.roots[dep.label] = dep[NativeLinkTargetInfo]

def _omnibus_soname(ctx):
    linker_type = get_cxx_toolchain_info(ctx).linker_info.type
    return get_shared_library_name(linker_type, "omnibus")

def _create_dummy_omnibus(ctx: "context", extra_ldflags: [""] = []) -> "artifact":
    linker_type = get_cxx_toolchain_info(ctx).linker_info.type
    output = ctx.actions.declare_output(get_shared_library_name(linker_type, "omnibus-dummy"))
    cxx_link_shared_library(
        ctx,
        output,
        name = _omnibus_soname(ctx),
        links = [LinkArgs(flags = extra_ldflags)],
        category_suffix = "dummy_omnibus",
    )
    return output

def _link_deps(
        link_infos: {"label": LinkableNode.type},
        deps: ["label"]) -> ["label"]:
    """
    Return transitive deps required to link dynamically against the given deps.
    This will following through deps of statically linked inputs and exported
    deps of everything else (see https://fburl.com/diffusion/rartsbkw from v1).
    """

    def find_deps(node: "label"):
        return get_deps_for_link(link_infos[node], LinkStyle("shared"))

    return breadth_first_traversal_by(link_infos, deps, find_deps)

def _all_deps(
        link_infos: {"label": LinkableNode.type},
        roots: ["label"]) -> ["label"]:
    """
    Return all transitive deps from following the given nodes.
    """

    def find_transitive_deps(node: "label"):
        return link_infos[node].deps + link_infos[node].exported_deps

    all_deps = breadth_first_traversal_by(link_infos, roots, find_transitive_deps)

    return all_deps

def _create_root(
        ctx: "context",
        spec: OmnibusSpec.type,
        root: NativeLinkTargetInfo.type,
        output: "artifact",
        link_deps: ["label"],
        omnibus: "artifact",
        extra_ldflags: [""] = [],
        prefer_stripped_objects: bool.type = False) -> LinkedObject.type:
    """
    Link a root omnibus node.
    """

    inputs = []

    # Since we're linking against a dummy omnibus which has no symbols, we need
    # to make sure the linker won't drop it from the link or complain about
    # missing symbols.
    linker_type = get_cxx_toolchain_info(ctx).linker_info.type
    inputs.append(LinkInfo(
        pre_flags =
            get_no_as_needed_shared_libs_flags(linker_type) +
            get_ignore_undefined_symbols_flags(linker_type),
    ))

    # add native target link input
    inputs.append(root.link_info)

    # Add deps of the root to the link line.
    omnibus_is_already_added = False
    for dep in link_deps:
        node = spec.link_infos[dep]
        actual_link_style = get_actual_link_style(
            LinkStyle("shared"),
            node.preferred_linkage,
        )

        # If this dep needs to be linked statically, then link it directly.
        if actual_link_style != LinkStyle("shared"):
            inputs.append(get_link_info(
                node,
                actual_link_style,
                prefer_stripped = prefer_stripped_objects,
            ))
            continue

        # If this is another root.
        if dep in spec.roots:
            _, other_root = spec.roots[dep]

            # TODO(cjhopman): This should be passing structured linkables
            inputs.append(LinkInfo(pre_flags = [cmd_args(other_root)]))
            continue

        # If this node is in omnibus, just add that to the link line.
        if dep in spec.body:
            if not omnibus_is_already_added:
                inputs.append(LinkInfo(linkables = [SharedLibLinkable(lib = omnibus)]))
                omnibus_is_already_added = True
            continue

        # At this point, this should definitely be an excluded node.
        expect(dep in spec.excluded, str(dep))

        # We should have already handled statically linked nodes above.
        expect(actual_link_style == LinkStyle("shared"))
        inputs.append(get_link_info(node, actual_link_style))

    # link the rule
    return cxx_link_shared_library(
        ctx,
        output,
        name = root.name,
        links = [LinkArgs(flags = extra_ldflags), LinkArgs(infos = inputs)],
        identifier = root.name or output.short_path,
    )

def _create_undefined_symbols_argsfile(
        ctx: "context",
        symbol_files: ["artifact"],
        local_only: bool.type = False) -> "artifact":
    """
    Combine files with sorted lists of symbols names into an argsfile to pass
    to the linker to mark these symbols as undefined (e.g. `-m`).
    """
    output = ctx.actions.declare_output("__undefined_symbols__.argsfile")
    script = (
        "set -euo pipefail; " +
        'LC_ALL=C sort -S 10% -u -m "$@" | sed "s/^/-u/" > {}'
    )
    ctx.actions.run(
        [
            "/bin/bash",
            "-c",
            cmd_args(output.as_output(), format = script),
            "",
        ] +
        symbol_files,
        category = "omnibus_undefined_syms_argsfile",
        local_only = local_only,
    )
    return output

def _extract_global_symbols_from_link_args(
        ctx: "context",
        name: str.type,
        link_args: [["artifact", "resolved_macro", "cmd_args", str.type]],
        local_only: bool.type = False) -> "artifact":
    """
    Extract global symbols explicitly set in the given linker args (e.g.
    `-Wl,--export-dynamic-symbol=<sym>`).
    """

    # TODO(T110378137): This is ported from D24065414, but it might make sense
    # to explicitly tell Buck about the global symbols, rather than us trying to
    # extract it from linker flags (which is brittle).
    output = ctx.actions.declare_output(name)

    # We intentionally drop the artifacts referenced in the args when generating
    # the argsfile -- we just want to parse out symbol name flags and don't need
    # to materialize artifacts to do this.
    argsfile, macros = ctx.actions.write(name + ".args", link_args, allow_args = True)

    # TODO(T110378133): Make this work with other platforms.
    param = "--export-dynamic-symbol"
    pattern = "\\(-Wl,\\)\\?{}[,=]\\([^,]*\\)".format(param)

    # Used sed/grep to filter the symbol name from the relevant flags.
    # TODO(T110378130): As is the case in v1, we don't properly extract flags
    # from argsfiles embedded in existing args.
    script = (
        "set -euo pipefail; " +
        'cat "$@" | (grep -- \'{0}\' || [[ $? == 1 ]]) | sed \'s|{0}|\\2|\' | LC_ALL=C sort -S 10% -u > {{}}'
            .format(pattern)
    )
    ctx.actions.run(
        [
            "/bin/bash",
            "-c",
            cmd_args(output.as_output(), format = script).hidden(macros),
            "",
            argsfile,
        ],
        category = "omnibus_global_symbol_flags",
        local_only = local_only,
    )
    return output

_GLOBAL_SYMVERS_SH = """\
set -euo pipefail
echo "{" > "$1"
echo "  global:" >> "$1"
LC_ALL=C sort -S 10% -u -m "${@:2}" | awk '{print "    \\""$1"\\";"}' >> "$1"
echo "  local: *;" >> "$1"
echo "};" >> "$1"
"""

def _create_global_symbols_version_script(
        ctx: "context",
        roots: ["artifact"],
        excluded: ["artifact"],
        link_args: [["artifact", "resolved_macro", "cmd_args", str.type]]) -> "artifact":
    """
    Generate a version script exporting symbols from from the given objects and
    link args.
    """

    output = ctx.actions.declare_output("__global_symbols__.vers")
    cmd = [
        "/bin/bash",
        "-c",
        _GLOBAL_SYMVERS_SH,
        "",
        output.as_output(),
    ]

    # Get global symbols from roots.  We set a rule to do this per-rule, as
    # using a single rule to process all roots adds overhead to the critical
    # path of incremental flows (e.g. that only update a single root).
    cmd.extend([
        extract_symbol_names(
            ctx,
            root.basename + ".global_syms.txt",
            [root],
            dynamic = True,
            global_only = True,
            category = "omnibus_global_syms",
            identifier = root.basename,
        )
        for root in roots
    ])

    # TODO(T110378126): Processing all excluded libs together may get expensive.
    # We should probably split this up and operate on individual libs.
    if excluded:
        cmd.append(extract_symbol_names(
            ctx,
            "__excluded_libs__.global_syms.txt",
            excluded,
            dynamic = True,
            global_only = True,
            category = "omnibus_global_syms_excluded_libs",
        ))

    # Extract explicitly globalized symbols from linker args.
    cmd.append(_extract_global_symbols_from_link_args(
        ctx,
        "__global_symbols_from_args__.txt",
        link_args,
    ))

    ctx.actions.run(
        cmd,
        category = "omnibus_version_script",
    )

    return output

def _is_static_only(info: LinkableNode.type) -> bool.type:
    """
    Return whether this can only be linked statically.
    """
    return info.preferred_linkage == Linkage("static")

def _is_shared_only(info: LinkableNode.type) -> bool.type:
    """
    Return whether this can only be linked statically.
    """
    return info.preferred_linkage == Linkage("shared")

def _create_omnibus(
        ctx: "context",
        spec: OmnibusSpec.type,
        extra_ldflags: [""] = [],
        prefer_stripped_objects: bool.type = False) -> LinkedObject.type:
    inputs = []

    # Undefined symbols roots...
    non_body_root_undefined_syms = [
        extract_symbol_names(
            ctx,
            out.basename + ".undefined_syms.txt",
            [out],
            dynamic = True,
            global_only = True,
            undefined_only = True,
            category = "omnibus_undefined_syms",
            identifier = out.basename,
        )
        for label, (_, out) in spec.roots.items()
        if label not in spec.body
    ]
    if non_body_root_undefined_syms:
        argsfile = _create_undefined_symbols_argsfile(
            ctx,
            non_body_root_undefined_syms,
        )
        inputs.append(LinkInfo(pre_flags = [
            cmd_args(argsfile, format = "@{}"),
        ]))

    # Process all body nodes.
    deps = {}
    global_symbols_link_args = []
    for label in spec.body:
        # If this body node is a root, add the it's output to the link.
        if label in spec.roots:
            _, root = spec.roots[label]

            # TODO(cjhopman): This should be passing structured linkables
            inputs.append(LinkInfo(pre_flags = [cmd_args(root)]))
            continue

        node = spec.link_infos[label]

        # Otherwise add in the static input for this node.
        actual_link_style = get_actual_link_style(
            LinkStyle("static_pic"),
            node.preferred_linkage,
        )
        expect(actual_link_style == LinkStyle("static_pic"))
        body_input = get_link_info(
            node,
            actual_link_style,
            prefer_stripped = prefer_stripped_objects,
        )
        inputs.append(body_input)
        global_symbols_link_args.append(link_info_to_args(body_input))

        # Keep track of all first order deps of the omnibus monolith.
        for dep in node.deps + node.exported_deps:
            if dep not in spec.body:
                expect(dep in spec.excluded)
                deps[dep] = None

    # Now add deps of omnibus to the link
    for label in _link_deps(spec.link_infos, deps.keys()):
        node = spec.link_infos[label]
        actual_link_style = get_actual_link_style(
            LinkStyle("shared"),
            node.preferred_linkage,
        )
        inputs.append(get_link_info(
            node,
            actual_link_style,
            prefer_stripped = prefer_stripped_objects,
        ))

    # Add global symbols version script.
    global_sym_vers = _create_global_symbols_version_script(
        ctx,
        # Extract symols from roots...
        [out for _, out in spec.roots.values()],
        # ... and the shared libs from excluded nodes.
        [
            shared_lib.output
            for label in spec.excluded
            for shared_lib in spec.link_infos[label].shared_libs.values()
        ],
        # Extract explicit global symbol names from flags in all body link args.
        global_symbols_link_args,
    )
    inputs.append(LinkInfo(pre_flags = [
        "-Wl,--version-script",
        global_sym_vers,
    ]))

    soname = _omnibus_soname(ctx)
    linker_info = get_cxx_toolchain_info(ctx).linker_info
    return cxx_link_into_shared_library(
        ctx,
        soname,
        links = [LinkArgs(flags = extra_ldflags), LinkArgs(infos = inputs)],
        # TODO(T110378138): As with static C++ links, omnibus links are
        # currently too large for RE, so run them locally for now (e.g.
        # https://fb.prod.workplace.com/groups/buck2dev/posts/2953023738319012/).
        # NB: We explicitly pass a value here to override
        # the linker_info.link_libraries_locally that's used by `cxx_link_into_shared_library`.
        # That's because we do not want to apply the linking behavior universally,
        # just use it for omnibus.
        local_only = linker_info.link_binaries_locally,
        link_weight = linker_info.link_weight,
        identifier = soname,
    )

def _build_omnibus_spec(
        ctx: "context",
        graph: LinkableGraph.type) -> OmnibusSpec.type:
    """
    Divide transitive deps into excluded, root, and body nodes, which we'll
    use to link the various parts of omnibus.
    """

    # Build up the set of all nodes that we have to exclude from omnibus linking
    # (any node that is excluded will exclude all it's transitive deps).
    excluded = {
        label: None
        for label in _all_deps(
            graph.nodes,
            graph.excluded.keys() +
            # Exclude any body nodes which can't be linked statically.
            [
                label
                for label, info in graph.nodes.items()
                if (label not in graph.roots) and _is_shared_only(info)
            ],
        )
    }

    # Finalized root nodes, after removing any excluded roots.
    linker_type = get_cxx_toolchain_info(ctx).linker_info.type
    roots = {
        label: (root, ctx.actions.declare_output(value_or(root.name, get_default_shared_library_name(linker_type, label))))
        for label, root in graph.roots.items()
        if label not in excluded
    }

    # Find the deps of the root nodes.  These form the roots of the nodes
    # included in the omnibus link.
    first_order_root_deps = []
    for label in _link_deps(graph.nodes, flatten([r.deps for r, _ in roots.values()])):
        # We only consider deps which aren't *only* statically linked.
        if _is_static_only(graph.nodes[label]):
            continue

        # Don't include a root's dep onto another root.
        if label in roots:
            continue
        first_order_root_deps.append(label)

    # All body nodes.  These included all non-excluded body nodes and any non-
    # excluded roots which are reachable by these body nodes (since they will
    # need to be put on the link line).
    body = {
        label: None
        for label in _all_deps(graph.nodes, first_order_root_deps)
        if label not in excluded
    }

    return OmnibusSpec(
        excluded = excluded,
        roots = roots,
        body = body,
        link_infos = graph.nodes,
    )

def _ordered_roots(
        spec: OmnibusSpec.type) -> [("label", NativeLinkTargetInfo.type, "artifact", ["label"])]:
    """
    Return information needed to link the roots nodes in topo-sorted order.
    """

    # Calculate all deps each root node needs to link against.
    link_deps = {}
    for label, (root, _) in spec.roots.items():
        link_deps[label] = _link_deps(spec.link_infos, root.deps)

    # Used the link deps to create the graph of root nodes.
    root_graph = {
        node: [dep for dep in deps if dep in spec.roots]
        for node, deps in link_deps.items()
    }

    ordered_roots = []

    # Emit the root link info as a topo-sorted list, so that we generate root link
    # rules for dependencies before their dependents.
    for label in topo_sort(root_graph):
        root, output = spec.roots[label]
        deps = link_deps[label]
        ordered_roots.append((label, root, output, deps))

    return ordered_roots

def create_omnibus_libraries(
        ctx: "context",
        graph: LinkableGraph.type,
        extra_ldflags: [""] = [],
        prefer_stripped_objects: bool.type = False) -> OmnibusSharedLibraries.type:
    spec = _build_omnibus_spec(ctx, graph)

    # Create dummy omnibus
    dummy_omnibus = _create_dummy_omnibus(ctx, extra_ldflags)

    libraries = {}
    roots = {}

    # Link all root nodes against the dummy libomnibus lib.
    for label, root, output, link_deps in _ordered_roots(spec):
        shlib = _create_root(
            ctx,
            spec,
            root,
            output,
            link_deps,
            dummy_omnibus,
            extra_ldflags,
            prefer_stripped_objects,
        )
        if root.name != None:
            libraries[root.name] = shlib
        roots[label] = shlib

    # If we have body nodes, then link them into the monolithic libomnibus.so.
    if spec.body:
        omnibus = _create_omnibus(
            ctx,
            spec,
            extra_ldflags,
            prefer_stripped_objects,
        )
        libraries[_omnibus_soname(ctx)] = omnibus

    # For all excluded nodes, just add their regular shared libs.
    for label in spec.excluded:
        for name, lib in spec.link_infos[label].shared_libs.items():
            libraries[name] = lib

    return OmnibusSharedLibraries(libraries = libraries, roots = roots)
