def apple_bundle_config() -> {str.type: ""}:
    return {
        "_codesign_type": read_config("apple", "codesign_type_override", None),
        "_incremental_bundling_enabled": (read_config("apple", "incremental_bundling_enabled", "true").lower() == "true"),
    }
