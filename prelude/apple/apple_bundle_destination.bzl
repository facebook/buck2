load("@prelude//:paths.bzl", "paths")

# Abstraction of a place in a resulting bundle where file or directory will be copied. Actual value
# of path relative to bundle root depends on a platform. This class is an implementation detail and
# is not exposed to user unlike `AppleResourceDestination`.
# v1 code is `com/facebook/buck/apple/AppleBundleDestination.java`
AppleBundleDestination = enum(
    "resources",
    "frameworks",
    "executables",
    "plugins",
    "xpcservices",
    "metadata",
    "watchapp",
    "headers",
    "modules",
    "quicklook",
    "watchkitstub",
    "bundleroot",
    "loginitems",
)

AppleBundleDestinationPaths = record(
    resources = field(str.type, ""),
    frameworks = field(str.type, ""),
    executables = field(str.type, ""),
    plugins = field(str.type, ""),
    xpcservices = field(str.type, ""),
    metadata = field(str.type, ""),
    watchapp = field(str.type, ""),
    headers = field(str.type, ""),
    modules = field(str.type, ""),
    quicklook = field(str.type, ""),
    watchkitstub = field(str.type, ""),
    bundleroot = field(str.type, ""),
    loginitems = field(str.type, ""),
)

_IOSBundleDestinationPaths = AppleBundleDestinationPaths(
    frameworks = "Frameworks",
    plugins = "PlugIns",
    xpcservices = "XPCServices",
    watchapp = "Watch",
    quicklook = "Library/QuickLook",
    watchkitstub = "_WatchKitStub",
)

_IOSFrameworkBundleDestinationPaths = AppleBundleDestinationPaths(
    frameworks = "Frameworks",
    xpcservices = "XPCServices",
    headers = "Headers",
    modules = "Modules",
)

macOS_content_path = "Contents"
_MacOSBundleDestinationPaths = AppleBundleDestinationPaths(
    resources = paths.join(macOS_content_path, "Resources"),
    frameworks = paths.join(macOS_content_path, "Frameworks"),
    executables = paths.join(macOS_content_path, "MacOS"),
    plugins = paths.join(macOS_content_path, "PlugIns"),
    xpcservices = paths.join(macOS_content_path, "XPCServices"),
    metadata = macOS_content_path,
    watchapp = macOS_content_path,
    headers = macOS_content_path,
    modules = macOS_content_path,
    quicklook = paths.join(macOS_content_path, "Library/QuickLook"),
    watchkitstub = macOS_content_path,
    bundleroot = macOS_content_path,
    loginitems = paths.join(macOS_content_path, "Library/LoginItems"),
)

_MacOSFrameworkBundleDestinationPaths = AppleBundleDestinationPaths(
    resources = "Resources",
    frameworks = "Frameworks",
    xpcservices = "XPCServices",
    metadata = "Resources",
    headers = "Headers",
    modules = "Modules",
)

def _get_apple_bundle_destinations_for_sdk_name(name: str.type) -> AppleBundleDestinationPaths.type:
    if name == "macosx" or name == "maccatalyst":
        return _MacOSBundleDestinationPaths
    else:
        return _IOSBundleDestinationPaths

def _get_apple_framework_bundle_destinations_for_sdk_name(name: str.type) -> AppleBundleDestinationPaths.type:
    if name == "macosx" or name == "maccatalyst":
        return _MacOSFrameworkBundleDestinationPaths
    else:
        return _IOSFrameworkBundleDestinationPaths

def bundle_relative_path_for_destination(destination: AppleBundleDestination.type, sdk_name: str.type, extension: str.type) -> str.type:
    if extension == "framework":
        bundle_destinations = _get_apple_framework_bundle_destinations_for_sdk_name(sdk_name)
    else:
        bundle_destinations = _get_apple_bundle_destinations_for_sdk_name(sdk_name)

    if destination.value == "resources":
        return bundle_destinations.resources
    elif destination.value == "frameworks":
        return bundle_destinations.frameworks
    elif destination.value == "executables":
        return bundle_destinations.executables
    elif destination.value == "plugins":
        return bundle_destinations.plugins
    elif destination.value == "xpcservices":
        return bundle_destinations.xpcservices
    elif destination.value == "metadata":
        return bundle_destinations.metadata
    elif destination.value == "watchapp":
        return bundle_destinations.watchapp
    elif destination.value == "headers":
        return bundle_destinations.headers
    elif destination.value == "modules":
        return bundle_destinations.modules
    elif destination.value == "quicklook":
        return bundle_destinations.quicklook
    elif destination.value == "watchkitstub":
        return bundle_destinations.watchkitstub
    elif destination.value == "bundleroot":
        return bundle_destinations.bundleroot
    elif destination.value == "loginitems":
        return bundle_destinations.loginitems
    fail("Unsupported Apple bundle destination {}".format(destination))
