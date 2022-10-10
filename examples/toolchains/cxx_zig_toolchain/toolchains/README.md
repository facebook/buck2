This example tests the `zig cc` based self-contained C/C++ toolchain.

To build it within the open source tree of buck2 to you need to
* Create a symlink for the prelude
  ```
  ln -s ../../../prelude prelude
  ```
* Remove the top-level `.buckconfig`
  ```
  rm ../../../.buckconfig
  ```
* Apply the following patch to the prelude
  ```
  diff --git a/prelude/cxx/tools/TARGETS.v2 b/prelude/cxx/tools/TARGETS.v2
  index 2030d2f..5db1689 100644
  --- a/prelude/cxx/tools/TARGETS.v2
  +++ b/prelude/cxx/tools/TARGETS.v2
  @@ -1,4 +1,3 @@
  -load("@fbcode_macros//build_defs/lib:python_common.bzl", "get_ldflags", "get_strip_mode")
   load(":defs.bzl", "cxx_hacks", "omnibus_environment")

   prelude = native
  @@ -538,7 +537,7 @@ omnibus_environment(
       # We override the binary-level ldflags with library level ldfalgs for
       # shared roots. We include 2 important things: stripping binaries, and not
       # discarding GPU code.
  -    shared_root_ld_flags = get_ldflags("", "", "omnibus_root", get_strip_mode("", ""), "") + [
  +    shared_root_ld_flags = [
           "-Wl,--no-discard-section=.nv_fatbin",
           "-Wl,--no-discard-section=.nvFatBinSegment",
           # Reorder nv_fatbin after the bss section to avoid overflow
  ```
