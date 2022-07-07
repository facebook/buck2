load(":apple_bundle_config.bzl", "apple_bundle_config")
load(":apple_info_plist_substitutions_parsing.bzl", "parse_codesign_entitlements")

_RESOURCE_BUNDLE_FIELDS = [
    "asset_catalogs_compilation_options",
    "binary",
    "deps",
    "extension",
    "ibtool_flags",
    "ibtool_module_flag",
    "info_plist",
    "info_plist_substitutions",
    "product_name",
    "resource_group",
    "resource_group_map",
]

def apple_bundle_macro_impl(apple_bundle_rule = None, apple_resource_bundle_rule = None, info_plist_substitutions = None, **kwargs):
    kwargs.update(apple_bundle_config())

    resource_bundle_target_name = None

    # Only enable resource bundle rule split if there's a separate toolchain,
    # otherwise both rules will resolve to the same toolchain and it's just
    # additional overhead without any benefits.
    resources_toolchain_enabled = (read_config("apple", "resources_toolchain", None) != None)
    if resources_toolchain_enabled:
        resource_bundle_name = kwargs["name"] + "__ResourceBundle_Private"
        resource_bundle_kwargs = {}
        for field_name in _RESOURCE_BUNDLE_FIELDS:
            resource_bundle_kwargs[field_name] = kwargs.get(field_name)

        # TODO(T125269558): Remove usage of apple_resource_bundle() once we have exec groups.
        apple_resource_bundle_rule(
            name = resource_bundle_name,
            **resource_bundle_kwargs
        )

        resource_bundle_target_name = ":{}".format(resource_bundle_name)

    # Splitting the resource compilation into another rule means we can have
    # different exec platforms for the resource compilation and for the rest
    # of the bundling process. This allows us to send resource compilations
    # directly to RE.
    #
    # +-------------------------------------------------+
    # |                 apple_bundle()                  |
    # |           Exec Platform: macOS/Linux            |
    # |   +--------+  +--------+ +------------------+   |
    # +---+ binary +--+  deps  +-+ _resource_bundle +---+
    #     +--------+  +--------+ +------------------+
    #          |           |               |
    #          |           |               |
    #          |           |               +---------------+
    #          |           |                               |
    #          |           |                               |
    #          |           |                               v
    #          |           |              +---------------------------------+
    #          |           +-----+        |     apple_resource_bundle()     |
    #          |                 |        |    Exec Platform: macOS-only    |
    #          |                 |        |     +--------+  +--------+      |
    #          |                 |        +-----+ binary +--+  deps  +------+
    #          |                 |              +--------+  +--------+
    #          |                 |                   |           |
    #          |                 |                   |           |
    #          |                 v                   |           |
    #          |       +-------------------+         |           |
    #          |       |   Dependencies    |<--------+-----------+
    #          |       +-------------------+         |
    #          |       +-------------------+         |
    #          +------>|      Binary       |<--------+
    #                  +-------------------+
    apple_bundle_rule(
        info_plist_substitutions = info_plist_substitutions,
        _codesign_entitlements = parse_codesign_entitlements(info_plist_substitutions),
        _resource_bundle = resource_bundle_target_name,
        **kwargs
    )
