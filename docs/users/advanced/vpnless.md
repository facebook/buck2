---
id: vpnless
title: Migrating to VPNless builds
---

Along with the wider [VPNless effort](https://fb.workplace.com/groups/1415703449254519) to enable full development on corporate laptops/desktops sans VPN, buck2 has been updated to work completely without VPN access.

> NOTE: VPNless operation is _only_ supported for buck2; buck1 will not work vpnless.

## Migration guide

To migrate your build to work VPNless, you'll need to do a few things:

1. Join [VPNless Local Dev Testers](https://fb.workplace.com/groups/1415703449254519) so you get enrolled in all the relevant GKs to enable VPNless.

2. Wait a bit for permissions to propagate. You may try running chef on your laptop with `sudo soloctl -i` to speed things up after joining the group.

3. Drop off VPN (or lighthouse); verify you're connected to your non-corporate network.

4. Restart buck2 on your laptop so you pick up the new configuration:
```[bash]
$ buck2 kill
```

5. Run your build, verify it works. Congratulations, you're building VPNless 😎

## Troubleshooting

Many builds should Just Work without VPN / lighthouse, but sometimes certain build rules need to be updated.

### I just need to build!

If you're running into a hard blocker and need to do a build even while gated into VPNless development, simply hop back onto VPN/Lighthouse and re-run your build. You may need to run `buck2 clean` in order to get to a working state again.

### Errors you may see
You may see failures like the following when performing a VPNless build the first time:

```
$ buck2 build //my/cool:project
Action failed: fbsource//xplat/toolchains/apple:xcode_14.3.1_14e300b-macosx-sdk_genrule (download_file archive.tar.zst)
Internal error: Received invalid sha1 digest. Expected eaaf6bd1951ee7200ca66d608dc837e28883ec1e, got bcac2ddc59677d169098742206a7c110fa00a385 from https://our.intern.facebook.com/intern/managed_sdk/component/272435095345806/; perhaps this url is not gated into vpnless?
Action failed: fbsource//xplat/toolchains/apple:xcode_14.3.1_14e300b_swift-resource-dir_macos (download_file archive.tar.zst)
Internal error: Received invalid sha1 digest. Expected f7cc25942c4e8a56f87c851ad75e351e71fae49a, got e63063370b23229764fa0d61e8fc6bbfe7137c16 from https://our.intern.facebook.com/intern/managed_sdk/component/271406345550923/; perhaps this url is not gated into vpnless?
Action failed: fbsource//xplat/toolchains/apple:xcode_14.3.1_14e300b_macosx-swift-resource-dir_macos (download_file archive.tar.zst)
Internal error: Received invalid sha1 digest. Expected b6bd2a7faba6985ccd2dd541efdeb48dfb82da31, got 93c6003bc10513445a20ec89d0843419192bd02d from https://our.intern.facebook.com/intern/managed_sdk/component/259997920045153/; perhaps this url is not gated into vpnless?
Action failed: fbsource//xplat/toolchains/apple:xcode_14.3.1_14e300b_linker-resource-dir_macos (download_file archive.tar.zst)
Internal error: Received invalid sha1 digest. Expected 145cde8a2d9c65f2bdd66818970b94c9ae974b3f, got 395b70b84a5e3ee8a12bed3cd0c31968fb37b94b from https://our.intern.facebook.com/intern/managed_sdk/component/786106529582223/; perhaps this url is not gated into vpnless?
Action failed: fbsource//xplat/toolchains/apple:xcode_14.3.1_14e300b_clang-resource-dir_macos (download_file archive.tar.zst)
Internal error: Received invalid sha1 digest. Expected f32f8356ed50596d5c000018573e9fd5d2836e48, got c4346966bd9c976f44e951f7e3974e8d2d0f3e86 from https://our.intern.facebook.com/intern/managed_sdk/component/921533932454562/; perhaps this url is not gated into vpnless?
BUILD FAILED
```

The `"perhaps this url is not gated into vpnless?"` should clue you in that this URL is unauthorized for VPNless.

You may also see errors like:

```
$ buck2 build //my/cool:project
Action failed: fbcode//third-party-java/commons-logging:commons-logging__1.2.jar (download_file commons-logging__1.2.jar)
Internal error: Error performing http_head request: HTTP: Error routing through x2p: Failed to connect to `manifold.facebook.net`; is it allowed on vpnless?
Buck UI: https://www.internalfb.com/buck2/fb694d27-b52d-4349-97f5-8082b24ec8f8
Note:    Using experimental modern dice
Network: Up: 280KiB  Down: 2.9MiB  (reSessionID-dab2bfcd-8b67-445a-b68f-0283619cc5da)
Jobs completed: 5739. Time elapsed: 20.3s.
Cache hits: 99%. Commands: 729 (cached: 725, remote: 2, local: 2)
BUILD FAILED
```

Again, `"is it allowed on vpnless?"` indicates this is likely a VPNless URL problem.

The most common issue we've seen is using a URL or endpoint that's not allowlisted for VPN access. This includes common endpoints like `our.intern.facebook.com`, `interngraph.intern.facebook.com`, or `manifold.facebook.net`.

The majority of broken rules will be something like [`remote_file`](https://www.internalfb.com/code/fbsource/fbcode/buck2/prelude/remote_file.bzl) or [`http_archive`](https://www.internalfb.com/code/fbsource/fbcode/buck2/prelude/http_archive/http_archive.bzl) rules which download from one of these three host. Instead, we have a slightly different way to tell buck which URL to use.

### Everstore contents

Many builds download remote Everstore artifacts via custom intern-hosted endpoints. [Android screenshot test fixtures](https://fb.workplace.com/groups/1415703449254519/posts/1463410231150507) are a good example. They have a `remote_file` call that looks like:
```[python]
fb_native.remote_file(
    name = t.file_target.target_name,
    out = t.file_target.output_name,
    sha1 = t.file_target.sha1,
    url = "https://our.intern.facebook.com/intern/screenshot_tests/resource/" + t.file_target.handle,
)
```

The `our.intern.facebook.com` URL is not authorized for VPNless access. Instead, you'll need to provide a new `vpnless_url` attribute.

Most `remote_file` calls like this have an associated Everstore handle (that looks something like `GPN0ZxTT3RXSQqkAAOhsHRepy5dGbjRYAA`). Assuming you have this handle, you can use our [VPNless helper macro library](https://www.internalfb.com/code/fbsource/tools/build_defs/buck2/vpnless.bzl) to make migration easier:

```[python]
load("fbsource//tools/build_defs/buck2:vpnless.bzl", "vpnless")

fb_native.remote_file(
    name = t.file_target.target_name,
    out = t.file_target.output_name,
    sha1 = t.file_target.sha1,
    url = "https://our.intern.facebook.com/intern/screenshot_tests/resource/" + t.file_target.handle,
+   vpnless_url = vpnless.everstore_url(t.file_target.handle),
)
```

Once you update your broken targets, try rebuilding and see if that works.

### Manifold contents

Manifold is very similar. If you have a `remote_file` rule referencing Manifold paths, consider switching to [`manifold_get` from the build_defs Manifold macros](https://www.internalfb.com/code/fbsource/tools/build_defs/manifold.bzl) which has already been updated to provide the `vpnless_url` attribute.

### Everything else

Some other things we've seen break are:
* MSDK bumps that rely on generated `.bzl` files in the repo. The code generator has been updated to add the `vpnless_url` attribute, you simply need to run a bump of your MSDK entry.

For any other questions or concerns about builds on VPNless, please post in [VPNless Local Dev Testers](https://fb.workplace.com/groups/1415703449254519).
