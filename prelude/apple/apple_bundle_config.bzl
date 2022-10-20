def _maybe_get_bool(config: str.type, default: [None, bool.type]) -> [None, bool.type]:
    result = read_config("apple", config, None)
    if result == None:
        return default
    return result.lower() == "true"

def apple_bundle_config() -> {str.type: ""}:
    return {
        "_codesign_type": read_config("apple", "codesign_type_override", None),
        "_incremental_bundling_enabled": _maybe_get_bool("incremental_bundling_enabled", True),
    }
