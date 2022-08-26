load("@prelude//utils:utils.bzl", "expect")

_RE_ENABLED = "supports_remote_execution"
_RE_OPTS_LABEL_PREFIX = "re_opts_capabilities="
_RE_OPTS_KEYS = ["platform", "subplatform"]

def _parse_re_opts(labels: [str.type]) -> [{str.type: str.type}, None]:
    """
    Parse out JSON-embedded RE options like:
    "re_opts_capabilities={\"platform\": \"gpu-remote-execution\", \"subplatform\": \"P100\"}"
    """

    for label in labels:
        if label.startswith(_RE_OPTS_LABEL_PREFIX):
            result = json.decode(label[len(_RE_OPTS_LABEL_PREFIX):])
            for key in result.keys():
                expect(key in _RE_OPTS_KEYS, "unexpected key in RE options label: {}", key)
            return result

    return None

# TODO(agallagher): Parsing RE options via JSON embedded in labels isn't a great
# UI, and we just do it here to support existing use cases.  Ideally, though, we'd
# present a better UI (e.g. an `re_opts` param for tests) and use that instead.
def get_re_executor_from_labels(labels: [str.type]) -> ["command_executor_config_builder", None]:
    """
    Parse legacy RE-enablement test labels and use them to configure a test RE
    executor to run the test with.

    The UI is best documented at:
    https://www.internalfb.com/intern/wiki/Remote_Execution/Users/GPU_RE_Contbuild_Migration/
    """

    # If the special "RE enabled" label isn't present, abort.
    if _RE_ENABLED not in labels:
        return None

    # If there's no options found in labels, don't use RE.  This diverges from
    # v1 behavior, but v2+tpx needs some platform to be set and so we probably
    # want to the toolchain tp provide some exec-platform compatible platform.
    re_opts = _parse_re_opts(labels)
    if re_opts == None:
        return None

    return CommandExecutorConfig(
        local_enabled = False,
        remote_enabled = True,
        remote_execution_properties = re_opts,
        remote_execution_use_case = "tpx-default",
    )
