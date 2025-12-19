# Erlang Toolchain Version Generation

This directory contains utilities for generating OTP application version information required for creating self-contained Erlang releases with bundled ERTS (`include_erts=True`).

## Quick Start

### 1. Generate Version Information

Run the script to extract version information from your Erlang installation:

```bash
python3 buck2/prelude/erlang/toolchain/generate_otp_versions.py my_otp_versions.bzl
```

This creates a `.bzl` file containing version information for all OTP applications.

### 2. Use in Your BUCK File

Load the generated file and configure your toolchain:

```python
load(":my_otp_versions.bzl", "get_otp_applications", "get_erts_version")

erlang_toolchain(
    name = "erlang-27",
    applications = get_otp_applications(),
    erts_version = get_erts_version(),
    otp_binaries = "//path/to:otp-binaries",
    # ... other configuration
)
```

### 3. Enable Bundled ERTS

Now you can create self-contained releases:

```python
erlang_release(
    name = "myapp",
    applications = [":myapp"],
    include_erts = True,  # Bundles ERTS in the release
)
```

## Why is This Needed?

Self-contained releases with bundled ERTS require versioned directory structures (e.g., `kernel-10.1` instead of `kernel-*`) to create proper OTP releases. This ensures:

- **Reproducibility**: Exact versions are locked in the build
- **Portability**: Release doesn't depend on system-installed Erlang
- **Isolation**: Each release bundles its own runtime

For releases without bundled ERTS (`include_erts=False`, the default), version information is optional and the system will use wildcard patterns.

## Files

- **`generate_otp_versions.py`**: Python script that extracts version info from OTP
- **`otp_versions.bzl`**: Generated OTP version information for the current Erlang installation

## Updating Versions

When you upgrade your Erlang/OTP installation, regenerate the version file:

```bash
$ python3 buck2/prelude/erlang/toolchain/generate_otp_versions.py my_otp_versions.bzl
$ git add my_otp_versions.bzl
$ git commit -m "Update OTP versions for Erlang/OTP 28"
```

## Troubleshooting

### Error: "include_erts=True requires explicit OTP application versions"

This means you're trying to create a bundled release without configuring version information. Follow the Quick Start steps above.

### Error: "Could not find version for OTP application: foo"

The application `foo` is not in your generated version file. Either:
1. Regenerate the file with your current Erlang installation
2. The application is not part of OTP (third-party app) - depend on it directly instead

### Different Erlang Versions for Different Targets

You can generate multiple version files for different Erlang installations:

```bash
# For OTP 27
$ /path/to/otp27/bin/python3 generate_otp_versions.py otp27_versions.bzl

# For OTP 26
$ /path/to/otp26/bin/python3 generate_otp_versions.py otp26_versions.bzl
```

Then create separate toolchains for each:

```python
load(":otp27_versions.bzl", otp27_apps = "get_otp_applications", otp27_erts = "get_erts_version")
load(":otp26_versions.bzl", otp26_apps = "get_otp_applications", otp26_erts = "get_erts_version")

erlang_toolchain(
    name = "erlang-27",
    applications = otp27_apps(),
    erts_version = otp27_erts(),
    ...
)

erlang_toolchain(
    name = "erlang-26",
    applications = otp26_apps(),
    erts_version = otp26_erts(),
    ...
)
```

## Technical Details

The generated `.bzl` file contains:
- List of all OTP applications with exact versions (e.g., `{"name": "kernel", "version": "10.1"}`)
- ERTS version (e.g., `"15.1"`)
- OTP release number (e.g., `"27"`)

This information is used by:
- `erlang_otp_application.bzl`: To create versioned app directories
- `erlang_release.bzl`: To create proper ERTS directory structure
- `boot_script_builder.erl`: To generate boot scripts with correct versions
