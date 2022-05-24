load(
    "@fbcode//buck2/prelude/cxx:compile.bzl",
    "CxxExtension",  # @unused Used as a type
    "CxxSrcWithFlags",  # @unused Used as a type
)

def cxx_populate_xcode_attributes(
        ctx,
        srcs: [CxxSrcWithFlags.type],
        argsfiles_by_ext: {CxxExtension.type: "artifact"},
        product_name: str.type) -> {str.type: ""}:
    converted_srcs = {}
    for src in srcs:
        file_properties = _get_artifact_owner(src.file)
        if src.flags:
            # List of resolved_macros will encode as:
            # [['\"-some-flag\"'], ['\"-another-flag\"']]
            #
            # Convert it to a string and rip-out the quotes
            # so it appears as ["-some-flag", "-another-flag"]
            file_properties["flags"] = [str(flag).replace('\"', "") for flag in src.flags]
        converted_srcs[src.file] = file_properties

    data = {
        "argsfiles_by_ext": {
            # Enum types cannot be encoded by our JSON API.
            # Use the str representation.
            repr(ext).replace('\"', ""): artifact
            for ext, artifact in argsfiles_by_ext.items()
        },
        "headers": _get_artifacts_with_owners(ctx.attr.headers),
        "product_name": product_name,
        "srcs": converted_srcs,
    }

    if hasattr(ctx.attr, "exported_headers"):
        data["exported_headers"] = _get_artifacts_with_owners(ctx.attr.exported_headers)

    return data

def _get_artifacts_with_owners(files: "") -> {"artifact": {str.type: "target_label"}}:
    if type(files) == "dict":
        return {artifact: _get_artifact_owner(artifact) for _, artifact in files.items()}
    else:
        return {file: _get_artifact_owner(file) for file in files}

def _get_artifact_owner(file: "artifact") -> {str.type: "target_label"}:
    if file.owner:
        return {"target": file.owner.raw_target()}
    else:
        return {}
