# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import asyncio
import hashlib
import re
import socket
from typing import Dict, List, Optional, Tuple

from aiohttp import web
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase, parametrize
from buck2.tests.e2e_util.helper.http_server import sha1_hex, StaticHttpServer
from buck2.tests.e2e_util.helper.utils import random_string


def configs(
    url: str,
    *,
    sha1: Optional[str] = None,
    sha256: Optional[str] = None,
    sha512: Optional[str] = None,
    size: Optional[int] = None,
) -> List[str]:
    """`-c` arguments declaring the download the fixture's targets read from the config."""
    args = ["-c", f"test.url={url}"]
    if sha1 is not None:
        args += ["-c", f"test.sha1={sha1}"]
    if sha256 is not None:
        args += ["-c", f"test.sha256={sha256}"]
    if sha512 is not None:
        args += ["-c", f"test.sha512={sha512}"]
    if size is not None:
        args += ["-c", f"test.size_bytes={size}"]
    return args


def sha256_hex(content: bytes) -> str:
    return hashlib.sha256(content).hexdigest()


ALGORITHMS = ["sha1", "sha256", "sha512"]


def digest(algorithm: str, content: bytes) -> str:
    return hashlib.new(algorithm, content).hexdigest()


def wrong_digest(algorithm: str) -> str:
    """A well-formed digest of `algorithm` that no content has."""
    return "0" * len(digest(algorithm, b""))


def declare(content: bytes, *algorithms: str) -> Dict[str, str]:
    """The `configs` keyword arguments declaring `content`'s digest in each of `algorithms`."""
    return {algorithm: digest(algorithm, content) for algorithm in algorithms}


# What a rule that names only a sha1 and/or a sha256 gets is part of buck2's interface. The
# expectations below are written out in full, not computed by the code under test.
LEGACY_CONTENT = b"download_file legacy checksum fixture\n"
LEGACY_SHA1 = "10fde9893af2c4d19c7a406b5db0de4695dff0d6"
LEGACY_SHA256 = "ae9eef44af54a0623acd40d91538a61a99bf2d4067c0cc64cdf3814c0efc9458"


# Which checksums the rule passes to `download_file`. Every combination of the three
# algorithms is a declaration that has to download and verify.
SHAPES = [
    ("sha1",),
    ("sha256",),
    ("sha512",),
    ("sha1", "sha256"),
    ("sha1", "sha512"),
    ("sha256", "sha512"),
    ("sha1", "sha256", "sha512"),
]


# Each case names the algorithm whose digest is wrong and the algorithms declared next to it,
# which are right.
WRONG_DIGEST_CASES = [
    (wrong, others)
    for wrong in ALGORITHMS
    for others in ([], [a for a in ALGORITHMS if a != wrong])
]


# A checksum is part of an action, and an action is part of an analysis result, which can be
# paged out to disk and back while a daemon runs.
PAGING = {
    "buck2_hydration": {"enable_paging": "true", "page_out_min_free_disk_gb": "0"}
}


def use_fbsource_digests(buck: Buck) -> None:
    """fbsource prefers BLAKE3-KEYED and allows SHA1, so a checksum's algorithm is not the one
    the download would hash with."""
    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("[buck2]\ndigest_algorithms = BLAKE3-KEYED,SHA1\n")


async def build_and_read(buck: Buck, target: str, *args: str) -> bytes:
    result = await buck.build(target, *args)
    return result.get_build_report().output_for_target(target).read_bytes()


class DownloadFileTest(BuckTestCase):
    @buck_test(data_dir="download", skip_for_os=["windows"])
    @parametrize(
        "shape", ["sha1", "sha256", "both", "both_with_size", "sha1_uppercase"]
    )
    async def test_downloads_declared_each_way(self, buck: Buck, shape: str) -> None:
        content = random_string().encode()
        sha1 = sha1_hex(content)
        sha256 = sha256_hex(content)
        declared = {
            "sha1": configs("", sha1=sha1),
            "sha256": configs("", sha256=sha256),
            "both": configs("", sha1=sha1, sha256=sha256),
            "both_with_size": configs("", sha1=sha1, sha256=sha256, size=len(content)),
            "sha1_uppercase": configs("", sha1=sha1.upper()),
        }[shape][2:]
        async with StaticHttpServer({"/file": content}) as server:
            assert (
                await build_and_read(
                    buck,
                    "//:copy",
                    "--local-only",
                    *configs(server.url("/file")),
                    *declared,
                )
                == content
            )
            assert server.count("GET", "/file") == 1

    @buck_test(data_dir="download")
    async def test_retries_transient_errors(self, buck: Buck) -> None:
        routes = web.RouteTableDef()

        attempt = 0
        body: bytes = random_string().encode()

        @routes.get("/")
        async def hello(request: web.Request) -> web.Response:
            nonlocal attempt
            attempt += 1
            if attempt > 2:
                return web.Response(body=body)
            if attempt > 1:
                return web.Response(status=500)
            return web.Response(status=429)

        app = web.Application()
        app.add_routes(routes)

        sock = socket.socket()
        sock.bind(("localhost", 0))

        runner = web.AppRunner(app)
        await runner.setup()
        site = web.SockSite(runner, sock)
        await site.start()

        port = sock.getsockname()[1]
        await buck.build(
            "//:download", *configs(f"http://localhost:{port}", sha1=sha1_hex(body))
        )

        await runner.cleanup()

        # HEAD, then the GET's two retried errors and its success.
        assert attempt == 4

    @buck_test(data_dir="download")
    async def test_a_server_without_head_support(self, buck: Buck) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}, allow_head=False) as server:
            assert (
                await build_and_read(
                    buck,
                    "//:download",
                    *configs(server.url("/file"), sha1=sha1_hex(content)),
                )
                == content
            )
            assert server.count("HEAD", "/file") == 1
            assert server.count("GET", "/file") == 1

    @buck_test(data_dir="download")
    async def test_times_out_after_retries(self, buck: Buck) -> None:
        routes = web.RouteTableDef()

        body: bytes = random_string().encode()
        sha1 = sha1_hex(body)

        @routes.get("/always_times_out")
        async def always_times_out(request: web.Request) -> web.Response:
            await asyncio.sleep(3)
            return web.Response(body=body)

        attempt = 0

        @routes.get("/times_out_twice")
        async def times_out_twice(request: web.Request) -> web.Response:
            nonlocal attempt
            attempt += 1
            if attempt > 2:
                return web.Response(body=body)
            await asyncio.sleep(3)
            return web.Response(body=body)

        app = web.Application()
        app.add_routes(routes)

        sock = socket.socket()
        sock.bind(("localhost", 0))

        runner = web.AppRunner(app)
        await runner.setup()
        site = web.SockSite(runner, sock)
        await site.start()

        port = sock.getsockname()[1]
        url = f"http://localhost:{port}"

        # These are daemon startup configs, need these to be written in a buckconfig rather
        # than passed as an invocation config.
        #
        # Add an aggressive read timeout.
        with open(buck.cwd / ".buckconfig", "a") as buckconfig:
            buckconfig.write("[http]\nread_timeout_ms = 50\n")

        await expect_failure(
            buck.build("//:download", *configs(f"{url}/always_times_out", sha1=sha1)),
            stderr_regex="Timed out while making request to",
        )

        result = await buck.build(
            "//:download", *configs(f"{url}/times_out_twice", sha1=sha1)
        )
        assert "Retrying a HTTP error after" in result.stderr

        await runner.cleanup()

    @buck_test(data_dir="download")
    async def test_a_missing_url_and_a_dead_server(self, buck: Buck) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            await expect_failure(
                buck.build(
                    "//:download", *configs(server.url("/gone"), sha1=sha1_hex(content))
                ),
                stderr_regex="404 Not Found",
            )
            # Nothing listens on port 1.
            await expect_failure(
                buck.build(
                    "//:download",
                    *configs("http://localhost:1/file", sha1=sha1_hex(content)),
                ),
                stderr_regex="Error performing http_download request|Connection refused",
            )

    @buck_test(data_dir="download")
    async def test_declarations_that_cannot_be_right(self, buck: Buck) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            url = server.url("/file")
            await expect_failure(
                buck.build("//:download", *configs(url)),
                stderr_regex="Must pass in at least one checksum",
            )
            await expect_failure(
                buck.build("//:download", *configs(url, sha1="xxxxxx")),
                stderr_regex="Invalid digest for `sha1` argument",
            )
            # Nothing was fetched for those.
            assert server.count("GET", "/file") == 0

    @buck_test(data_dir="download")
    async def test_checksums_the_content_does_not_match(self, buck: Buck) -> None:
        content = random_string().encode()
        zeros40 = "0" * 40
        zeros64 = "0" * 64
        async with StaticHttpServer({"/file": content}) as server:
            url = server.url("/file")
            await expect_failure(
                buck.build("//:download", *configs(url, sha1=zeros40)),
                stderr_regex="Invalid sha1 digest",
            )
            await expect_failure(
                buck.build(
                    "//:download", *configs(url, sha1=zeros40, size=len(content))
                ),
                stderr_regex="Invalid sha1 digest",
            )
            await expect_failure(
                buck.build("//:download", *configs(url, sha256=zeros64)),
                stderr_regex="Invalid sha256 digest",
            )
            # Content the CAS does not have is downloaded, and every checksum named is held
            # against it.
            await expect_failure(
                buck.build(
                    "//:download", *configs(url, sha1=sha1_hex(content), sha256=zeros64)
                ),
                stderr_regex="Invalid sha256 digest",
            )

    @buck_test(data_dir="download")
    async def test_a_declared_size_of_zero_is_reported_by_the_download(
        self, buck: Buck
    ) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            # A checksum paired with a size of 0 is not the empty file, so the CAS is not asked
            # about it: the download runs and reports the mismatch like any other wrong size.
            await expect_failure(
                buck.build(
                    "//:download",
                    *configs(server.url("/file"), sha1=sha1_hex(content), size=0),
                ),
                stderr_regex=f"Downloaded size \\({len(content)}\\) does not match expected size \\(0\\)",
            )
            assert server.count("GET", "/file") == 1

    @buck_test(data_dir="download")
    async def test_a_wrong_size_is_reported_by_the_download(self, buck: Buck) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            await expect_failure(
                buck.build(
                    "//:download",
                    *configs(
                        server.url("/file"),
                        sha1=sha1_hex(content),
                        size=len(content) + 1,
                    ),
                ),
                stderr_regex=f"Downloaded size \\({len(content)}\\) does not match expected size \\({len(content) + 1}\\)",
            )

    @buck_test(data_dir="download", skip_for_os=["windows"])
    async def test_a_content_based_path_under_a_different_preferred_digest(
        self,
        buck: Buck,
    ) -> None:
        # The output's value is what consumers resolve a content-based path from, so it has to be
        # the value the path was built from: the declared one, under the checksum's algorithm.
        use_fbsource_digests(buck)
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            assert (
                await build_and_read(
                    buck,
                    "//:copy_content_based",
                    "--local-only",
                    *configs(server.url("/file"), sha1=sha1_hex(content)),
                )
                == content
            )

    @buck_test(data_dir="download")
    @parametrize("digest_algorithm", ["SHA1", "SHA256"])
    async def test_fetches_once_across_restarts(
        self, buck: Buck, digest_algorithm: str
    ) -> None:
        with open(buck.cwd / ".buckconfig", "a") as f:
            f.write("[buck2]\n")
            f.write(f"digest_algorithms = {digest_algorithm}\n")
            f.write("sqlite_materializer_state = true\n")

        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            declared = configs(
                server.url("/file"), sha1=sha1_hex(content), sha256=sha256_hex(content)
            )
            target = "//:download"

            res = await buck.build(target, *declared)
            output = res.get_build_report().output_for_target(target)
            assert output.read_bytes() == content
            assert server.count("GET", "/file") == 1

            # The file is still on disk, so a new daemon has no reason to fetch it again.
            await buck.kill()
            await buck.build(target, *declared)
            assert output.read_bytes() == content
            assert server.count("GET", "/file") == 1

    @buck_test(data_dir="download")
    async def test_is_uploaded_from_disk(self, buck: Buck) -> None:
        content = random_string().encode()
        # Force the uploader to treat the download as missing from the CAS even if it is there.
        buck.set_env(
            "BUCK2_TEST_INJECTED_MISSING_DIGESTS",
            f"{sha1_hex(content)}:{len(content)}",
        )
        async with StaticHttpServer({"/file": content}) as server:
            await buck.build(
                "//:copy",
                "--remote-only",
                "--no-remote-cache",
                *configs(server.url("/file"), sha1=sha1_hex(content)),
            )
            assert server.count("GET", "/file") == 1

    @buck_test(data_dir="download")
    async def test_uses_the_cas_when_it_has_the_content(self, buck: Buck) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            declared = configs(server.url("/file"), sha1=sha1_hex(content))

            # Running an RE action on the download uploads its content to the CAS.
            await buck.build("//:copy", "--remote-only", "--no-remote-cache", *declared)
            assert server.count("GET", "/file") == 1

            # A second download of the same content finds it in the CAS, so it neither contacts
            # the server nor touches the disk until something needs the file...
            target = "//:download_again"
            res = await buck.build(target, "--materializations=none", *declared)
            output = res.get_build_report().output_for_target(target)
            assert not output.exists()
            assert server.count("GET", "/file") == 1

            # ...and then it comes out of the CAS.
            await buck.build(target, *declared)
            assert output.read_bytes() == content
            assert server.count("GET", "/file") == 1

    @buck_test(data_dir="download")
    async def test_content_the_cas_holds_needs_no_url(self, buck: Buck) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            # With a size declared, the download is fully described without contacting the URL.
            declared = configs(
                server.url("/file"), sha1=sha1_hex(content), size=len(content)
            )
            await buck.build("//:copy", "--remote-only", "--no-remote-cache", *declared)
            assert server.count("GET", "/file") == 1

            # A new daemon and a URL nothing answers on: the declaration names content the CAS
            # holds, so a remote consumer and a local one both get it without the URL.
            await buck.kill()
            broken = configs(
                "http://localhost:1/file", sha1=sha1_hex(content), size=len(content)
            )
            await buck.build("//:copy", "--remote-only", "--no-remote-cache", *broken)
            assert (
                await build_and_read(buck, "//:copy_again", "--local-only", *broken)
                == content
            )
            assert server.count("GET", "/file") == 1

    @buck_test(data_dir="download")
    async def test_a_second_checksum_is_not_checked_against_content_the_cas_serves(
        self,
        buck: Buck,
    ) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            await buck.build(
                "//:copy",
                "--remote-only",
                "--no-remote-cache",
                *configs(server.url("/file"), sha1=sha1_hex(content)),
            )
            assert server.count("GET", "/file") == 1

            # The CAS vouches for the sha1 and serves the content under it; the sha256 the
            # declaration also names is never held against bytes nobody fetches.
            assert (
                await build_and_read(
                    buck,
                    "//:copy_again",
                    "--local-only",
                    *configs(
                        server.url("/file"), sha1=sha1_hex(content), sha256="0" * 64
                    ),
                )
                == content
            )
            assert server.count("GET", "/file") == 1

    @buck_test(data_dir="download")
    async def test_a_wrong_size_is_a_miss_in_the_cas(self, buck: Buck) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            await buck.build(
                "//:copy",
                "--remote-only",
                "--no-remote-cache",
                *configs(server.url("/file"), sha1=sha1_hex(content)),
            )
            assert server.count("GET", "/file") == 1

            # The CAS names content by hash and size, so the content is not there under the wrong
            # size: the download runs and reports the mismatch.
            await expect_failure(
                buck.build(
                    "//:download_again",
                    *configs(
                        server.url("/file"),
                        sha1=sha1_hex(content),
                        size=len(content) + 1,
                    ),
                ),
                stderr_regex=f"Downloaded size \\({len(content)}\\) does not match expected size \\({len(content) + 1}\\)",
            )
            assert server.count("GET", "/file") == 2

    @buck_test(data_dir="download")
    @parametrize("shape", ["sha1", "sha256", "both", "both_uppercase"])
    async def test_legacy_checksums_download_the_same_bytes(
        self, buck: Buck, shape: str
    ) -> None:
        declared = {
            "sha1": configs("", sha1=LEGACY_SHA1),
            "sha256": configs("", sha256=LEGACY_SHA256),
            "both": configs("", sha1=LEGACY_SHA1, sha256=LEGACY_SHA256),
            "both_uppercase": configs(
                "", sha1=LEGACY_SHA1.upper(), sha256=LEGACY_SHA256.upper()
            ),
        }[shape][2:]
        async with StaticHttpServer({"/file": LEGACY_CONTENT}) as server:
            output = await build_and_read(
                buck, "//:download", *configs(server.url("/file")), *declared
            )
            assert output == LEGACY_CONTENT
            assert sha1_hex(output) == LEGACY_SHA1
            assert sha256_hex(output) == LEGACY_SHA256
            assert server.count("GET", "/file") == 1

    @buck_test(data_dir="download")
    async def test_legacy_wrong_digests_fail_with_the_same_messages(
        self, buck: Buck
    ) -> None:
        zeros40 = "0" * 40
        zeros64 = "0" * 64
        async with StaticHttpServer({"/file": LEGACY_CONTENT}) as server:
            url = server.url("/file")
            await expect_failure(
                buck.build("//:download", *configs(url, sha1=zeros40)),
                stderr_regex=re.escape(
                    f"Invalid sha1 digest. Expected {zeros40}, got {LEGACY_SHA1}. URL: {url}"
                ),
            )
            await expect_failure(
                buck.build("//:download", *configs(url, sha256=zeros64)),
                stderr_regex=re.escape(
                    f"Invalid sha256 digest. Expected {zeros64}, got {LEGACY_SHA256}. URL: {url}"
                ),
            )
            await expect_failure(
                buck.build(
                    "//:download", *configs(url, sha1=LEGACY_SHA1, sha256=zeros64)
                ),
                stderr_regex=re.escape(
                    f"Invalid sha256 digest. Expected {zeros64}, got {LEGACY_SHA256}. URL: {url}"
                ),
            )
            await expect_failure(
                buck.build(
                    "//:download", *configs(url, sha1=zeros40, sha256=LEGACY_SHA256)
                ),
                stderr_regex=re.escape(
                    f"Invalid sha1 digest. Expected {zeros40}, got {LEGACY_SHA1}. URL: {url}"
                ),
            )

    @buck_test(data_dir="download")
    async def test_legacy_malformed_digests_fail_with_the_same_messages(
        self, buck: Buck
    ) -> None:
        async with StaticHttpServer({"/file": LEGACY_CONTENT}) as server:
            url = server.url("/file")
            await expect_failure(
                buck.build("//:download", *configs(url)),
                stderr_regex="Must pass in at least one checksum",
            )
            await expect_failure(
                buck.build("//:download", *configs(url, sha1="xxxxxx")),
                stderr_regex=re.escape(
                    "Invalid digest for `sha1` argument, expected length of 40 but got 6, digest `xxxxxx`"
                ),
            )
            await expect_failure(
                buck.build("//:download", *configs(url, sha256="z" * 64)),
                stderr_regex=re.escape(
                    "Invalid digest for `sha256` argument, expected 0-9 a-z hex characters, but got `z`"
                ),
            )
            assert server.count("GET", "/file") == 0

    @buck_test(data_dir="download", skip_for_os=["windows"])
    @parametrize("shape", SHAPES, ids=["+".join(s) for s in SHAPES])
    async def test_downloads_with_every_combination_of_checksums(
        self, buck: Buck, shape: Tuple[str, ...]
    ) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            output = await build_and_read(
                buck,
                "//:download",
                *configs(server.url("/file"), **declare(content, *shape)),
            )
            assert output == content
            assert server.count("GET", "/file") == 1

    @buck_test(data_dir="download", skip_for_os=["windows"])
    async def test_a_sha512_in_capitals_is_accepted(self, buck: Buck) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            output = await build_and_read(
                buck,
                "//:download",
                *configs(server.url("/file"), sha512=digest("sha512", content).upper()),
            )
            assert output == content

    @buck_test(data_dir="download", skip_for_os=["windows"])
    async def test_a_sha512_with_the_right_size_downloads(self, buck: Buck) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            output = await build_and_read(
                buck,
                "//:download",
                *configs(
                    server.url("/file"),
                    **declare(content, "sha512"),
                    size=len(content),
                ),
            )
            assert output == content
            assert server.count("GET", "/file") == 1

    @buck_test(data_dir="download", skip_for_os=["windows"])
    @parametrize(
        "wrong,others",
        WRONG_DIGEST_CASES,
        ids=[
            f"{w}_wrong_with_{'+'.join(o) or 'nothing'}" for w, o in WRONG_DIGEST_CASES
        ],
    )
    async def test_a_wrong_digest_of_each_kind_fails_the_build(
        self, buck: Buck, wrong: str, others: List[str]
    ) -> None:
        content = random_string().encode()
        declared = declare(content, *others)
        declared[wrong] = wrong_digest(wrong)
        async with StaticHttpServer({"/file": content}) as server:
            url = server.url("/file")
            await expect_failure(
                buck.build("//:download", *configs(url, **declared)),
                stderr_regex=re.escape(
                    f"Invalid {wrong} digest. Expected {wrong_digest(wrong)}, "
                    f"got {digest(wrong, content)}. URL: {url}"
                ),
            )
            # The content was fetched and then refused: the check is on the bytes.
            assert server.count("GET", "/file") == 1

    @buck_test(data_dir="download", skip_for_os=["windows"])
    async def test_a_sha512_that_is_not_a_digest_is_refused_before_any_download(
        self, buck: Buck
    ) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            url = server.url("/file")
            too_short = digest("sha256", content)
            await expect_failure(
                buck.build("//:download", *configs(url, sha512=too_short)),
                stderr_regex=re.escape(
                    "Invalid digest for `sha512` argument, expected length of 128 "
                    f"but got 64, digest `{too_short}`"
                ),
            )
            not_hex = "z" * 128
            await expect_failure(
                buck.build("//:download", *configs(url, sha512=not_hex)),
                stderr_regex=re.escape(
                    "Invalid digest for `sha512` argument, expected 0-9 a-z hex "
                    "characters, but got `z`"
                ),
            )
            assert server.count("GET", "/file") == 0

    @buck_test(data_dir="download", skip_for_os=["windows"])
    async def test_a_sha512_alone_checks_the_declared_size(self, buck: Buck) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            await expect_failure(
                buck.build(
                    "//:download",
                    *configs(
                        server.url("/file"),
                        **declare(content, "sha512"),
                        size=len(content) + 1,
                    ),
                ),
                stderr_regex=f"Downloaded size \\({len(content)}\\) does not match expected size \\({len(content) + 1}\\)",
            )

    @buck_test(
        data_dir="download",
        skip_for_os=["windows"],
        write_invocation_record=True,
        extra_buck_config=PAGING,
    )
    @parametrize("shape", SHAPES, ids=["+".join(s) for s in SHAPES])
    async def test_a_paged_out_download_is_read_back(
        self, buck: Buck, shape: Tuple[str, ...]
    ) -> None:
        content = random_string().encode()
        async with StaticHttpServer({"/file": content}) as server:
            args = configs(server.url("/file"), **declare(content, *shape))
            assert await build_and_read(buck, "//:download", *args) == content

            await buck.debug("hydration", "page-out")
            status = (await buck.debug("hydration", "status")).stdout
            paged_out = re.search(r"(\d+) paged out", status)
            assert paged_out is not None and int(paged_out.group(1)) > 0, status

            # A target that reads the download needs its analysis result back, which holds the
            # action and with it the checksum.
            result = await buck.build("//:copy", "--local-only", *args)
            output = result.get_build_report().output_for_target("//:copy").read_bytes()
            assert output == content
            stats = result.invocation_record().get("page_in_by_key_type", {})
            analysis = stats.get("AnalysisKey")
            assert analysis is not None and int(analysis["count"]) > 0, list(stats)
            assert server.count("GET", "/file") == 1
