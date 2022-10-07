# Provider which exposes a field from `apple_binary` to `apple_bundle` as it might be used during code signing.
AppleEntitlementsInfo = provider(fields = [
    # Optional "artifact"
    "entitlements_file",
])

CodeSignType = enum(
    "skip",
    "adhoc",
    "distribution",
)
