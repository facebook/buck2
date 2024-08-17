load("@prelude//apple:versions.bzl", "TARGET_SDK_VERSIONS")
load("@prelude//:native.bzl", "native")

def generate_sdk_versions():
  native.constraint_setting(
      name = "constraint-setting-target-sdk-version",
      visibility = ["PUBLIC"],
  )

  for ver in TARGET_SDK_VERSIONS:
    native.constraint_value(
      name = "constraint-value-target-sdk-version-" + ver,
      constraint_setting = ":constraint-setting-target-sdk-version",
      visibility = ["PUBLIC"]
    )
