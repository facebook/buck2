AppleAssetCatalogsCompilationOptions = record(
    enable_notices = field(bool.type),
    enable_warnings = field(bool.type),
    enable_errors = field(bool.type),
    compress_pngs = field(bool.type),
    optimization = field(str.type),
    output_format = field(str.type),
    extra_flags = field([str.type]),
)

def get_apple_asset_catalogs_compilation_options(ctx: "context") -> AppleAssetCatalogsCompilationOptions.type:
    options = ctx.attr.asset_catalogs_compilation_options

    return AppleAssetCatalogsCompilationOptions(
        enable_notices = options.get("notices", True),
        enable_warnings = options.get("warnings", True),
        enable_errors = options.get("errors", True),
        compress_pngs = options.get("compress_pngs", True),
        optimization = options.get("optimization", "space"),
        output_format = options.get("output_format", "human-readable-text"),
        extra_flags = options.get("extra_flags", []),
    )
