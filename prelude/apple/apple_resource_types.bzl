# Represents the values for the `destination` field of `apple_resource`
AppleResourceDestination = enum(
    "resources",
    "frameworks",
    "executables",
    "plugins",
    "xpcservices",
)

# Defines _where_ resources need to be placed in an `apple_bundle`
AppleResourceSpec = record(
    # TODO(T110378104): Add support for `variants`, `codesign_on_copy`
    files = field(["artifact"], []),
    dirs = field(["artifact"], []),
    content_dirs = field(["artifact"], []),
    destination = AppleResourceDestination.type,
    variant_files = field(["artifact"], []),
    # Map from locale to list of files for that locale, e.g.
    # `{ "ru.lproj" : ["Localizable.strings"] }`
    named_variant_files = field({str.type: ["artifact"]}, {}),
)
