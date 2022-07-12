load("@fbcode//buck2/prelude/java:java_providers.bzl", "KeystoreInfo")

def keystore_impl(ctx: "context") -> ["provider"]:
    sub_targets = {}
    sub_targets["keystore"] = [DefaultInfo(default_outputs = [ctx.attrs.store])]
    sub_targets["properties"] = [DefaultInfo(default_outputs = [ctx.attrs.properties])]

    return [
        KeystoreInfo(store = ctx.attrs.store, properties = ctx.attrs.properties),
        DefaultInfo(default_outputs = [ctx.attrs.store, ctx.attrs.properties], sub_targets = sub_targets),
    ]
