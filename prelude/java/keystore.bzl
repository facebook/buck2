load("@fbcode//buck2/prelude/java:java_providers.bzl", "KeystoreInfo")

def keystore_impl(ctx: "context") -> ["provider"]:
    sub_targets = {}
    sub_targets["keystore"] = [DefaultInfo(default_outputs = [ctx.attr.store])]
    sub_targets["properties"] = [DefaultInfo(default_outputs = [ctx.attr.properties])]

    return [
        KeystoreInfo(store = ctx.attr.store, properties = ctx.attr.properties),
        DefaultInfo(default_outputs = [ctx.attr.store, ctx.attr.properties], sub_targets = sub_targets),
    ]
