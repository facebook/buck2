# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//utils:type_defs.bzl", "is_select")
load("@shim//build_defs/lib:oss.bzl", "default_base_module", "translate_target")

def _build_addopts(pytest_config, pytest_marks, pytest_expr, pytest_confcutdir):
    parts = []
    if pytest_config:
        parts += ["-c", pytest_config]
    if pytest_marks:
        parts += ["-m", pytest_marks]
    if pytest_expr:
        parts += ["-k", pytest_expr]
    if pytest_confcutdir:
        parts += ["--confcutdir", pytest_confcutdir]
    return parts

def _src_to_module(src, base_module):
    if not src.endswith(".py"):
        fail("python_pytest sources must be .py files: {}".format(src))
    relative = src[:-3].replace("/", ".")
    return base_module + "." + relative if base_module else relative

def _shell_single_quote(s):
    return "'" + s.replace("'", "'\\''") + "'"

def _translate_env_value_str(v):
    # Rewrite any cell-qualified targets that appear inside `$(macro ...)`
    # substrings (e.g. `$(location fbcode//buck2:buck2)`) so that env values
    # produced by the Meta-internal `tests/buck_e2e.bzl` resolve correctly
    # through the OSS shim cell mapping.
    if "//" not in v:
        return v
    chunks = v.split("$(")
    out = [chunks[0]]
    for chunk in chunks[1:]:
        if ")" not in chunk:
            out.append("$(" + chunk)
            continue
        end = chunk.index(")")
        macro_body = chunk[:end]
        rest = chunk[end:]
        tokens = macro_body.split(" ")
        for i in range(len(tokens)):
            if "//" in tokens[i]:
                tokens[i] = translate_target(tokens[i])
        out.append("$(" + " ".join(tokens) + rest)
    return "".join(out)

def _translate_env_value(v):
    if is_select(v):
        return select_map(v, _translate_env_value_str)
    return _translate_env_value_str(v)

def python_pytest(
        name,
        srcs = [],
        env = None,
        emails = None,
        base_module = None,
        deps = None,
        resources = None,
        pytest_config = None,
        pytest_marks = None,
        pytest_expr = None,
        pytest_confcutdir = None,
        skip_on_mode_mac = False,
        skip_on_mode_win = False,
        modifiers = None,
        **kwargs):
    # The shim has no native handling for these meta-internal attrs; tests
    # silently run on all platforms in OSS.
    _unused = (skip_on_mode_mac, skip_on_mode_win, modifiers)  # @unused

    if deps != None:
        kwargs["deps"] = [translate_target(d) for d in deps]

    if resources != None:
        # Meta's `python_pytest` takes resources as `{target: dest_path}`;
        # the OSS `python_test` rule takes `{dest_path: target}`. Flip and
        # rewrite the target paths to their shim equivalents.
        kwargs["resources"] = {
            dest: translate_target(target)
            for target, dest in resources.items()
        }

    env = {k: _translate_env_value(v) for k, v in env.items()} if env else {}
    extra = _build_addopts(pytest_config, pytest_marks, pytest_expr, pytest_confcutdir)

    # The buck2 e2e tests use an async `buck` pytest fixture; pytest-asyncio's
    # default "strict" mode requires explicit fixture marking, which the
    # upstream tests don't have. Meta's internal harness sets this elsewhere.
    extra = ["--asyncio-mode=auto"] + extra
    if extra:
        existing = env.get("PYTEST_ADDOPTS", "").strip()
        joined = " ".join(extra)
        env["PYTEST_ADDOPTS"] = (existing + " " + joined) if existing else joined

    module_base = base_module if base_module != None else default_base_module()
    test_modules = [_src_to_module(s, module_base) for s in srcs]

    main_lines = [
        "import sys",
        "import pytest",
        "",
        "TEST_MODULES = [",
    ] + ['    "{}",'.format(m) for m in test_modules] + [
        "]",
        "",
        "if __name__ == \"__main__\":",
        "    sys.exit(pytest.main([\"--pyargs\"] + TEST_MODULES + sys.argv[1:]))",
    ]
    quoted_lines = " ".join([_shell_single_quote(l) for l in main_lines])

    main_genrule = "__{}__pytest_main".format(name)

    # @lint-ignore BUCKLINT: avoid "Direct usage of native rules is not allowed."
    native.genrule(
        name = main_genrule,
        out = "__pytest_main__.py",
        cmd = "printf '%s\\n' " + quoted_lines + " > $OUT",
    )

    if emails != None:
        kwargs["contacts"] = emails

    # The buck2 e2e tests look up `__manifest__.fbmake["build_rule"]` (a Meta
    # convention) to derive a stable isolation prefix per test. Provide a
    # minimally compatible shape so tests that read this don't crash in OSS.
    kwargs.setdefault(
        "manifest_module_entries",
        {"fbmake": {"build_rule": "{}:{}".format(native.package_name(), name)}},
    )

    # @lint-ignore BUCKLINT: avoid "Direct usage of native rules is not allowed."
    native.python_test(
        name = name,
        srcs = list(srcs) + [":" + main_genrule],
        env = env,
        base_module = module_base,
        main_module = (module_base + "." if module_base else "") + "__pytest_main__",
        **kwargs
    )
