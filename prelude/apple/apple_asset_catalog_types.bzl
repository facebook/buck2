StringWithSourceTarget = record(
    # Target providing the string value
    source = field("label"),
    value = field("string"),
)

AppleAssetCatalogSpec = record(
    # At most one per given `apple_bundle` (including all transitive catalog dependencies),
    # optional reference in a form of a name (extension omitted) of an .appiconset which
    # contains an image set representing an application icon.
    # This set should be contained in one of catalogs referenced by `dirs` attribute.
    app_icon = field([StringWithSourceTarget.type, None]),
    dirs = field(["artifact"]),
    # Same as `app_icon` but with an application launch image semantics.
    launch_image = field([StringWithSourceTarget.type, None]),
)

AppleAssetCatalogResult = record(
    # Directory which contains compiled assets ready to be copied into application bundle
    compiled_catalog = field("artifact"),
    # .plist file to be merged into main application Info.plist file, containing information about compiled assets
    catalog_plist = field("artifact"),
)
