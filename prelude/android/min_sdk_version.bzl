_MIN_SDK_VERSION = 19
_MAX_SDK_VERSION = 33

def get_min_sdk_version_constraint_value_name(min_sdk: int.type) -> str.type:
    return "min_sdk_version_{}".format(min_sdk)

def get_min_sdk_version_range() -> range.type:
    return range(_MIN_SDK_VERSION, _MAX_SDK_VERSION)
