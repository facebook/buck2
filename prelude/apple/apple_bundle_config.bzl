def apple_bundle_config() -> {str.type: ""}:
    return {
        "_codesign_type": read_config("apple", "codesign_type_override", None),
        "_compile_resources_locally": (read_config("apple", "compile_resources_locally", "false").lower() == "true"),
        "_incremental_bundling_enabled": (read_config("apple", "incremental_bundling_enabled", "false").lower() == "true"),
    }
