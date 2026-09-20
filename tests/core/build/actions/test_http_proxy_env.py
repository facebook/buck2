# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


from __future__ import annotations

import asyncio
import hashlib
import json
import uuid

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.http_server import StaticHttpServer


PROXY_ENV_VARS = [
    "HTTP_PROXY",
    "http_proxy",
    "HTTPS_PROXY",
    "https_proxy",
    "NO_PROXY",
    "no_proxy",
]


def make_payload() -> bytes:
    return f"download contents {uuid.uuid4()}\n".encode()


def serve(
    payload: bytes | dict[str, bytes],
    *,
    host: str = "127.0.0.1",
    redirects: dict[str, str] | None = None,
) -> StaticHttpServer:
    """
    A server on the host this file's allowlists name, answering every request with `payload`, or
    routing by what was asked for when given a mapping.
    """
    return StaticHttpServer(payload, host=host, redirects=redirects)


def configure(
    buck: Buck, allowlist: str | None = "127.0.0.1", digest: str = "SHA256"
) -> None:
    for name in PROXY_ENV_VARS:
        assert buck.get_env_var(name) is None, (
            f"{name} leaked into the test environment"
        )
    config = f"[buck2]\ndigest_algorithms = {digest}\n"
    if allowlist is not None:
        config += f"[http]\nproxy_env_allowlist = {allowlist}\n"
    (buck.cwd / ".buckconfig.local").write_text(config)


def build_args(
    payload: bytes,
    origin: StaticHttpServer,
    *,
    size: bool = False,
) -> list[str]:
    args = [
        "--local-only",
        "--no-remote-cache",
        "-c",
        f"test.url={origin.url('/download')}",
        "-c",
        f"test.sha256={hashlib.sha256(payload).hexdigest()}",
    ]
    if size:
        args.extend(["-c", f"test.size_bytes={len(payload)}"])
    return args


async def build_download(
    buck: Buck,
    payload: bytes,
    origin: StaticHttpServer,
    env: dict[str, str],
    *,
    size: bool = False,
) -> None:
    result = await asyncio.wait_for(
        buck.build(
            "//:download",
            *build_args(payload, origin, size=size),
            env=env,
        ),
        timeout=90,
    )
    output = result.get_build_report().output_for_target("root//:download")
    assert output.read_bytes() == payload


async def daemon_pid(buck: Buck, env: dict[str, str]) -> int:
    result = await asyncio.wait_for(buck.status(env=env), timeout=30)
    return json.loads(result.stdout)["process_info"]["pid"]


class TestHttpProxyEnv:
    @pytest.fixture(autouse=True)
    def clear_proxy_environment(self, monkeypatch: pytest.MonkeyPatch) -> None:
        for name in PROXY_ENV_VARS:
            monkeypatch.delenv(name, raising=False)

    @pytest.mark.parametrize(
        "allowlist",
        [None, "", "   "],
        ids=["unset", "empty", "whitespace"],
    )
    @buck_test(skip_for_os=["windows"])
    async def test_requires_client_opt_in(
        self, buck: Buck, allowlist: str | None
    ) -> None:
        configure(buck, allowlist)
        payload = make_payload()
        async with serve(payload) as origin, serve(payload) as proxy:
            await build_download(buck, payload, origin, {"HTTP_PROXY": proxy.url()})
            assert proxy.requests == []
            assert origin.requests == [
                ("HEAD", "/download"),
                ("GET", "/download"),
            ]

    @pytest.mark.parametrize(
        "digest,size,methods",
        [
            ("SHA1", False, ["GET"]),
            ("SHA256", False, ["HEAD", "GET"]),
            ("SHA256", True, ["GET"]),
        ],
        ids=["get-only", "head-then-get", "sized-get-only"],
    )
    @buck_test(skip_for_os=["windows"])
    async def test_proxy_download_paths(
        self, buck: Buck, digest: str, size: bool, methods: list[str]
    ) -> None:
        configure(buck, digest=digest)
        payload = make_payload()
        async with serve(payload) as origin, serve(payload) as proxy:
            await build_download(
                buck,
                payload,
                origin,
                {"HTTP_PROXY": proxy.url()},
                size=size,
            )
            assert origin.requests == []
            assert proxy.requests == [
                (method, origin.url("/download")) for method in methods
            ]

    @pytest.mark.parametrize(
        "values,proxied",
        [
            ({}, False),
            ({"http_proxy": "proxy"}, True),
            (
                {"HTTP_PROXY": "proxy", "http_proxy": "origin"},
                True,
            ),
            ({"HTTP_PROXY": "proxy", "NO_PROXY": "127.0.0.1"}, False),
            ({"HTTP_PROXY": "proxy", "no_proxy": "127.0.0.1"}, False),
            ({"NO_PROXY": "127.0.0.1"}, False),
            ({"SDK_PROXY": "proxy", "Http_Proxy": "proxy"}, False),
            ({"ALL_PROXY": "proxy", "all_proxy": "proxy"}, False),
        ],
        ids=[
            "all-unset",
            "lowercase",
            "uppercase-precedence",
            "no-proxy-exclusion",
            "lowercase-no-proxy-exclusion",
            "no-proxy-alone",
            "nonstandard-variables-ignored",
            "all-proxy-ignored",
        ],
    )
    @buck_test(skip_for_os=["windows"])
    async def test_proxy_environment_semantics(
        self, buck: Buck, values: dict[str, str], proxied: bool
    ) -> None:
        configure(buck)
        payload = make_payload()
        async with serve(payload) as origin, serve(payload) as proxy:
            substitutions = {"origin": origin.url(), "proxy": proxy.url()}
            env = {
                name: substitutions.get(value, value) for name, value in values.items()
            }
            await build_download(buck, payload, origin, env)
            if proxied:
                assert origin.requests == []
                assert proxy.requests == [
                    ("HEAD", origin.url("/download")),
                    ("GET", origin.url("/download")),
                ]
            else:
                assert proxy.requests == []
                assert origin.requests == [
                    ("HEAD", "/download"),
                    ("GET", "/download"),
                ]

    @pytest.mark.parametrize(
        "values",
        [
            {"HTTP_PROXY": ""},
            {"http_proxy": ""},
            {"HTTPS_PROXY": ""},
            {"https_proxy": ""},
            {"HTTP_PROXY": "not a valid proxy"},
        ],
        ids=[
            "empty-http",
            "empty-http-lowercase",
            "empty-https",
            "empty-https-lowercase",
            "malformed",
        ],
    )
    @buck_test(skip_for_os=["windows"])
    async def test_invalid_proxy_values_are_rejected(
        self, buck: Buck, values: dict[str, str]
    ) -> None:
        configure(buck)
        payload = make_payload()
        async with serve(payload) as origin:
            await asyncio.wait_for(
                expect_failure(
                    buck.build(
                        "//:download",
                        *build_args(payload, origin),
                        env=values,
                    ),
                    stderr_regex="Invalid (HTTP|HTTPS)_PROXY uri",
                ),
                timeout=90,
            )
            assert origin.requests == []

    @buck_test(skip_for_os=["windows"])
    async def test_empty_allowlist_ignores_invalid_proxy_values(
        self, buck: Buck
    ) -> None:
        configure(buck, "")
        payload = make_payload()
        async with serve(payload) as origin:
            await build_download(
                buck,
                payload,
                origin,
                {"HTTP_PROXY": "not a valid proxy", "HTTPS_PROXY": ""},
            )
            assert origin.requests == [
                ("HEAD", "/download"),
                ("GET", "/download"),
            ]

    @buck_test(skip_for_os=["windows"])
    async def test_same_daemon_routes_allowed_and_other_hosts(self, buck: Buck) -> None:
        configure(buck, " EXAMPLE.com. , 127.0.0.1 ")
        allowed_payload = make_payload()
        direct_payload = make_payload()
        async with (
            serve(allowed_payload) as allowed_origin,
            serve(direct_payload, host="localhost") as direct_origin,
            serve(allowed_payload) as proxy,
        ):
            env = {"HTTP_PROXY": proxy.url()}
            await build_download(buck, allowed_payload, allowed_origin, env)
            first_pid = await daemon_pid(buck, env)
            await build_download(buck, direct_payload, direct_origin, env)
            assert await daemon_pid(buck, env) == first_pid
            assert allowed_origin.requests == []
            assert direct_origin.requests == [
                ("HEAD", "/download"),
                ("GET", "/download"),
            ]
            assert proxy.requests == [
                ("HEAD", allowed_origin.url("/download")),
                ("GET", allowed_origin.url("/download")),
            ]

    @pytest.mark.parametrize("initial_allowed", [True, False])
    @buck_test(skip_for_os=["windows"])
    async def test_redirect_uses_destination_allowlist(
        self, buck: Buck, initial_allowed: bool
    ) -> None:
        configure(buck, digest="SHA1")
        payload = make_payload()
        initial_host = "127.0.0.1" if initial_allowed else "localhost"
        final_host = "localhost" if initial_allowed else "127.0.0.1"
        async with serve(payload, host=final_host) as final_origin:
            final_url = final_origin.url("/download")
            async with serve(
                payload, host=initial_host, redirects={"/download": final_url}
            ) as initial_origin:
                initial_url = initial_origin.url("/download")
                async with serve(payload, redirects={initial_url: final_url}) as proxy:
                    await build_download(
                        buck, payload, initial_origin, {"HTTP_PROXY": proxy.url()}
                    )
                    assert initial_origin.requests == (
                        [] if initial_allowed else [("GET", "/download")]
                    )
                    assert final_origin.requests == (
                        [("GET", "/download")] if initial_allowed else []
                    )
                    assert proxy.requests == [
                        ("GET", initial_url if initial_allowed else final_url)
                    ]

    @buck_test(skip_for_os=["windows"])
    async def test_proxy_environment_changes_require_manual_restart(
        self, buck: Buck
    ) -> None:
        configure(buck)
        payloads = [make_payload() for _ in range(3)]
        async with (
            serve(payloads[0]) as origin,
            serve(payloads[1]) as second_origin,
            serve(payloads[2]) as third_origin,
        ):
            origins = [origin, second_origin, third_origin]
            # Either proxy can be asked for any of the three, and answers as that origin would.
            routes = {
                server.url("/download"): payload
                for server, payload in zip(origins, payloads)
            }
            async with serve(routes) as first_proxy, serve(routes) as second_proxy:
                first_env = {"HTTP_PROXY": first_proxy.url()}
                await build_download(buck, payloads[0], origin, first_env)
                first_pid = await daemon_pid(buck, first_env)
                assert first_proxy.requests == [
                    ("HEAD", origin.url("/download")),
                    ("GET", origin.url("/download")),
                ]

                # A download in the same daemon, asked for with a different proxy in the
                # environment, still goes through the proxy the daemon started with.
                second_env = {"HTTP_PROXY": second_proxy.url()}
                await build_download(buck, payloads[1], second_origin, second_env)
                assert await daemon_pid(buck, second_env) == first_pid
                assert second_proxy.requests == []
                assert first_proxy.requests[-2:] == [
                    ("HEAD", second_origin.url("/download")),
                    ("GET", second_origin.url("/download")),
                ]

                # Only a restart picks the new one up.
                await asyncio.wait_for(buck.kill(), timeout=30)
                await build_download(buck, payloads[2], third_origin, second_env)
                assert await daemon_pid(buck, second_env) != first_pid
                assert second_proxy.requests == [
                    ("HEAD", third_origin.url("/download")),
                    ("GET", third_origin.url("/download")),
                ]
                assert [server.requests for server in origins] == [[], [], []]

    @buck_test(skip_for_os=["windows"])
    async def test_checkout_local_allowlist_restarts_existing_daemon(
        self, buck: Buck
    ) -> None:
        configure(buck, "127.0.0.2")
        first_payload = make_payload()
        payload = make_payload()
        async with serve(first_payload) as origin, serve(payload) as proxy:
            env = {"HTTP_PROXY": proxy.url()}
            await build_download(buck, first_payload, origin, env)
            disabled_pid = await daemon_pid(buck, env)
            assert proxy.requests == []

            configure(buck)
            result = await asyncio.wait_for(
                buck.build("//:download", *build_args(payload, origin), env=env),
                timeout=90,
            )
            output = result.get_build_report().output_for_target("root//:download")
            assert output.read_bytes() == payload
            assert await daemon_pid(buck, env) != disabled_pid
            assert proxy.requests == [
                ("HEAD", origin.url("/download")),
                ("GET", origin.url("/download")),
            ]

    @buck_test(skip_for_os=["windows"])
    async def test_empty_allowlist_ignores_proxy_environment_changes(
        self, buck: Buck
    ) -> None:
        configure(buck, "")
        payload = make_payload()
        async with (
            serve(payload) as origin,
            serve(payload) as first_proxy,
            serve(payload) as second_proxy,
        ):
            first_env = {"HTTP_PROXY": first_proxy.url()}
            await build_download(buck, payload, origin, first_env)
            first_pid = await daemon_pid(buck, first_env)
            second_env = {"HTTP_PROXY": second_proxy.url()}
            await build_download(buck, payload, origin, second_env)
            assert await daemon_pid(buck, second_env) == first_pid
            assert first_proxy.requests == []
            assert second_proxy.requests == []
