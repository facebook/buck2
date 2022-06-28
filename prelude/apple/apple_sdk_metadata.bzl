AppleSdkMetadata = record(
    name = field(str.type),
    target_device_flags = field([str.type], []),
    is_ad_hoc_code_sign_sufficient = field(bool.type),
    info_plist_supported_platforms_values = field([str.type]),
)

IPhoneOSSdkMetadata = AppleSdkMetadata(
    name = "iphoneos",
    target_device_flags = ["--target-device", "iphone", "--target-device", "ipad"],
    is_ad_hoc_code_sign_sufficient = False,
    info_plist_supported_platforms_values = ["iPhoneOS"],
)

IPhoneSimulatorSdkMetadata = AppleSdkMetadata(
    name = "iphonesimulator",
    target_device_flags = ["--target-device", "iphone", "--target-device", "ipad"],
    is_ad_hoc_code_sign_sufficient = True,
    info_plist_supported_platforms_values = ["iPhoneSimulator"],
)

TVOSSdkMetadata = AppleSdkMetadata(
    name = "appletvos",
    target_device_flags = ["--target-device", "tv"],
    is_ad_hoc_code_sign_sufficient = False,
    info_plist_supported_platforms_values = ["AppleTVOS"],
)

TVSimulatorSdkMetadata = AppleSdkMetadata(
    name = "appletvsimulator",
    target_device_flags = ["--target-device", "tv"],
    is_ad_hoc_code_sign_sufficient = True,
    info_plist_supported_platforms_values = ["AppleTVSimulator"],
)

WatchOSSdkMetadata = AppleSdkMetadata(
    name = "watchos",
    target_device_flags = ["--target-device", "watch"],
    is_ad_hoc_code_sign_sufficient = False,
    info_plist_supported_platforms_values = ["WatchOS"],
)

WatchSimulatorSdkMetadata = AppleSdkMetadata(
    name = "watchsimulator",
    target_device_flags = ["--target-device", "watch"],
    is_ad_hoc_code_sign_sufficient = True,
    info_plist_supported_platforms_values = ["WatchSimulator"],
)

MacOSXSdkMetadata = AppleSdkMetadata(
    name = "macosx",
    target_device_flags = ["--target-device", "mac"],
    is_ad_hoc_code_sign_sufficient = True,
    info_plist_supported_platforms_values = ["MacOSX"],
)

MacOSXCatalystSdkMetadata = AppleSdkMetadata(
    name = "maccatalyst",
    target_device_flags = ["--target-device", "ipad"],
    is_ad_hoc_code_sign_sufficient = True,
    info_plist_supported_platforms_values = ["MacOSX"],
)

_SDK_MAP = {
    IPhoneOSSdkMetadata.name: IPhoneOSSdkMetadata,
    IPhoneSimulatorSdkMetadata.name: IPhoneSimulatorSdkMetadata,
    TVOSSdkMetadata.name: TVOSSdkMetadata,
    TVSimulatorSdkMetadata.name: TVSimulatorSdkMetadata,
    WatchOSSdkMetadata.name: WatchOSSdkMetadata,
    WatchSimulatorSdkMetadata.name: WatchSimulatorSdkMetadata,
    MacOSXSdkMetadata.name: MacOSXSdkMetadata,
    MacOSXCatalystSdkMetadata.name: MacOSXCatalystSdkMetadata,
}

def get_apple_sdk_metadata_for_sdk_name(name: str.type) -> AppleSdkMetadata.type:
    sdk = _SDK_MAP.get(name)
    if sdk == None:
        fail("unrecognized sdk name: `{}`".format(name))
    return sdk
