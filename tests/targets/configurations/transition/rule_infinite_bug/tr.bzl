def _impl(platform, refs):
    _ = refs
    return PlatformInfo(
        # Increase the length of the label, so it overflows (incorrectly).
        label = platform.label + "!hello!",
        configuration = platform.configuration,
    )

transition_increase_label_len = transition(impl = _impl, refs = {})
