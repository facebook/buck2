load(":apple_bundle_destination.bzl", "AppleBundleDestination")
load(
    ":apple_resource_types.bzl",
    "AppleResourceDestination",  # @unused Used as a type
)

def apple_bundle_destination_from_resource_destination(res_destination: AppleResourceDestination.type) -> AppleBundleDestination.type:
    return AppleBundleDestination(res_destination.value)
