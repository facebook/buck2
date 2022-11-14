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
    files = field(["artifact"], []),
    dirs = field(["artifact"], []),
    content_dirs = field(["artifact"], []),
    destination = AppleResourceDestination.type,
    variant_files = field(["artifact"], []),
    # Map from locale to list of files for that locale, e.g.
    # `{ "ru.lproj" : ["Localizable.strings"] }`
    named_variant_files = field({str.type: ["artifact"]}, {}),
    codesign_files_on_copy = field(bool.type, False),
)

# Used when invoking `ibtool`, `actool` and `momc`
AppleResourceProcessingOptions = record(
    prefer_local = field(bool.type, False),
    allow_cache_upload = field(bool.type, False),
)
