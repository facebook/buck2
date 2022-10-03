def _cas_artifact_impl(ctx: "context"):
    out = ctx.actions.cas_artifact(
        ctx.label.name,
        ctx.attrs.digest,
        ctx.attrs.use_case,
        expires_after_timestamp = ctx.attrs.expires_after_timestamp,
    )
    return [DefaultInfo(default_outputs = [out])]

cas_artifact = rule(impl = _cas_artifact_impl, attrs = {
    "digest": attrs.string(),
    "expires_after_timestamp": attrs.int(),
    "use_case": attrs.string(),
})
