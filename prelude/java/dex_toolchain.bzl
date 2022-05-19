# TODO(T107163344) These should be part of the Android toolchain!
# Move out once we have overlays.
DexToolchainInfo = provider(
    "Dex toolchain info",
    fields = [
        "android_jar",
        "d8_command",
    ],
)
