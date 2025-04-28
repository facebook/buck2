manifest-merger 25.2.0

== Steps

1) Downloaded following sources from Maven central:
com.android.tools.build:manifest-merger:25.2.0
com.android.tools:common:25.2.0
com.android.tools:annotations:25.2.0
com.android.tools:sdk-common:25.2.0
com.android.tools:sdklib:25.2.0
com.android.tools.layoutlib:layoutlib-api:25.2.0
2) Copied com.android.manifmerger.ManifestMerger2 and all required dependencies to the aosp/ module.
3) In the Android SDK's source code manifmerger, sdklib and utils are separate modules. These are
copied into a single module which alters certain package paths. For example,
com.android.SDKConstants becomes com.android.common.SDKConstants. This is done to avoid conflicts
with different version of those libraries in //third-party/java/android
4) Applied patch from tools-targetApi.patch to fix https://github.com/facebook/buck/issues/2149
5) Fine-tuned the compression ratio so that the compression step does not take too long just to achieve
   a tiny bit of reduction in the size of the compressed file.
   Applied patch from SignedJarBuilder.patch.
6) Applied patch from tools-queriesSupport.patch to add support for merging manifests with <queries> tags
