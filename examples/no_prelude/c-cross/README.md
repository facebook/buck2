# C Cross-Compilation Example

Demonstrates a simple cross-compilation capable toolchain setup for C with Buck2.
The example works on Linux x86-64 and can either target x86-64 or cross compile to Windows x86-64.

To illustrate native compilation:
```
$ buck2 build //:x86_64-linux-main.exe --show-output
...
BUILD SUCCEEDED
self//:x86_64-linux-main.exe buck-out/v2/gen/self/3fab24d5c7ef89af/__main.exe__/main.exe
$ buck-out/v2/gen/self/3fab24d5c7ef89af/__main.exe__/main.exe
Hello, World!
```

To illustrate cross-compilation:
```
$ buck2 build //:x86_64-windows-main.exe --show-output
...
BUILD SUCCEEDED
self//:x86_64-windows-main.exe buck-out/v2/gen/self/995247b55e990dd5/__main.exe__/main.exe
$ wine64 buck-out/v2/gen/self/995247b55e990dd5/__main.exe__/main.exe
Hello, World!
```
