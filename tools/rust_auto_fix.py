#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
rust_auto_fix.py - Diagnose and fix common Rust third-party crate build issues in Buck2.

This script handles the most common failure modes:
  1. Missing vendor/ directory (source files not vendored)
  2. Missing BUCK.reindeer (build rules not generated)
  3. Per-crate source resolution failures (precise_srcs issues)
  4. Build script issues for crates with native code

Usage:
    python3 tools/rust_auto_fix.py                     # Full diagnostic + auto-fix
    python3 tools/rust_auto_fix.py <crate_name>        # Fix a specific crate
    python3 tools/rust_auto_fix.py --bootstrap          # Run full bootstrap (vendor + buckify)
    python3 tools/rust_auto_fix.py --check              # Diagnostic only, no changes

Examples:
    python3 tools/rust_auto_fix.py sorted_vector_map
    python3 tools/rust_auto_fix.py --bootstrap
"""

import os
import subprocess
import sys
from pathlib import Path

# Paths relative to repo root
REPO_ROOT = Path(__file__).resolve().parent.parent
THIRD_PARTY_RUST = REPO_ROOT / "shim" / "third-party" / "rust"
VENDOR_DIR = THIRD_PARTY_RUST / "vendor"
FIXUPS_DIR = THIRD_PARTY_RUST / "fixups"
BUCK_REINDEER = THIRD_PARTY_RUST / "BUCK.reindeer"
REINDEER_BIN = REPO_ROOT / "bootstrap" / "reindeer"

# License header for generated fixup files
LICENSE_HEADER = """\
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.
"""

# Registry of known crates that need fixups and what kind
# Format: crate_name -> list of fixup lines
KNOWN_FIXUPS = {
    # Crates where precise_srcs parsing fails (use glob instead)
    "sorted_vector_map": ["precise_srcs = false"],
    "windows-targets": ["precise_srcs = false"],
    "windows-sys": ["precise_srcs = false"],
    "proc-macro2": ["precise_srcs = false"],
    # Crates with build scripts that should not run in Buck2
    "libsqlite3-sys": ["buildscript.run = false"],
    "openssl-sys": ["buildscript.run = false"],
    "ring": ["buildscript.run = false"],
    "libz-sys": ["buildscript.run = false"],
}


class DiagnosticResult:
    """Collects diagnostic findings."""

    def __init__(self):
        self.errors = []
        self.warnings = []
        self.fixes_applied = []

    def error(self, msg):
        self.errors.append(msg)
        print(f"  ERROR: {msg}")

    def warn(self, msg):
        self.warnings.append(msg)
        print(f"  WARN:  {msg}")

    def fixed(self, msg):
        self.fixes_applied.append(msg)
        print(f"  FIXED: {msg}")

    @property
    def ok(self):
        return len(self.errors) == 0


def check_vendor_dir():
    """Check if the vendor directory exists and is populated."""
    if not VENDOR_DIR.exists():
        return False, "vendor/ directory does not exist"
    entries = list(VENDOR_DIR.iterdir())
    if not entries:
        return False, "vendor/ directory is empty"
    return True, f"vendor/ contains {len(entries)} crate(s)"


def check_buck_reindeer():
    """Check if BUCK.reindeer exists."""
    if not BUCK_REINDEER.exists():
        return False, "BUCK.reindeer does not exist (need to run reindeer buckify)"
    return True, "BUCK.reindeer exists"


def check_crate_vendored(crate_name):
    """Check if a specific crate's sources are vendored."""
    # Crate directories follow the pattern: <crate_name>-<version>
    matches = list(VENDOR_DIR.glob(f"{crate_name}-*")) if VENDOR_DIR.exists() else []
    if not matches:
        return False, f"No vendored sources found for {crate_name}"
    for match in matches:
        src_dir = match / "src"
        if src_dir.exists() and any(src_dir.glob("*.rs")):
            return True, f"Found vendored sources at {match.name}"
    return False, f"Vendored directory exists but has no Rust sources"


def create_fixup(crate_name, fixup_lines, dry_run=False):
    """Create a fixup.toml for a specific crate."""
    fixup_dir = FIXUPS_DIR / crate_name
    fixup_file = fixup_dir / "fixups.toml"

    if fixup_file.exists():
        # Read existing content to check if fixup is already present
        existing = fixup_file.read_text()
        already_present = all(line in existing for line in fixup_lines)
        if already_present:
            return False, f"Fixup already configured in {fixup_file.relative_to(REPO_ROOT)}"

    if dry_run:
        return True, f"Would create fixup at {fixup_file.relative_to(REPO_ROOT)}"

    fixup_dir.mkdir(parents=True, exist_ok=True)
    content = LICENSE_HEADER + "\n"
    for line in fixup_lines:
        content += f"{line}\n"

    fixup_file.write_text(content)
    return True, f"Created fixup at {fixup_file.relative_to(REPO_ROOT)}"


def run_reindeer(subcommand):
    """Run a reindeer subcommand."""
    cmd = [
        str(REINDEER_BIN),
        "--third-party-dir",
        str(THIRD_PARTY_RUST),
        subcommand,
    ]
    print(f"\n  Running: {' '.join(cmd)}")
    try:
        result = subprocess.run(
            cmd,
            cwd=str(REPO_ROOT),
            capture_output=True,
            text=True,
            timeout=600,  # 10 minute timeout for vendoring
        )
        if result.returncode != 0:
            print(f"  STDOUT: {result.stdout[-500:]}" if result.stdout else "")
            print(f"  STDERR: {result.stderr[-500:]}" if result.stderr else "")
            return False, f"reindeer {subcommand} failed (exit code {result.returncode})"
        return True, f"reindeer {subcommand} completed successfully"
    except FileNotFoundError:
        return False, (
            f"reindeer binary not found at {REINDEER_BIN}. "
            "Make sure dotslash is installed: https://dotslash-cli.com"
        )
    except subprocess.TimeoutExpired:
        return False, f"reindeer {subcommand} timed out after 10 minutes"


def diagnose(diag, crate_name=None):
    """Run diagnostics on the third-party Rust setup."""
    print("\n=== Diagnosing Buck2 Rust third-party build ===\n")

    # Check vendor directory
    ok, msg = check_vendor_dir()
    if ok:
        print(f"  OK:    {msg}")
    else:
        diag.error(msg)

    # Check BUCK.reindeer
    ok, msg = check_buck_reindeer()
    if ok:
        print(f"  OK:    {msg}")
    else:
        diag.error(msg)

    # Check specific crate if requested
    if crate_name:
        ok, msg = check_crate_vendored(crate_name)
        if ok:
            print(f"  OK:    {msg}")
        else:
            diag.error(msg)

        # Check if fixup exists
        fixup_file = FIXUPS_DIR / crate_name / "fixups.toml"
        if fixup_file.exists():
            print(f"  OK:    Fixup exists at {fixup_file.relative_to(REPO_ROOT)}")
        else:
            diag.warn(f"No fixup file for {crate_name}")


def fix_crate(crate_name, dry_run=False):
    """Apply known fixup for a specific crate."""
    diag = DiagnosticResult()

    print(f"\n=== Fixing crate: {crate_name} ===\n")

    # Step 1: Check if we have a known fixup
    fixup_lines = KNOWN_FIXUPS.get(crate_name)
    if fixup_lines:
        created, msg = create_fixup(crate_name, fixup_lines, dry_run=dry_run)
        if created:
            diag.fixed(msg)
        else:
            print(f"  SKIP:  {msg}")
    else:
        # Default: try precise_srcs = false as a general fix for source resolution
        print(f"  INFO:  No known fixup for '{crate_name}', trying precise_srcs = false")
        created, msg = create_fixup(crate_name, ["precise_srcs = false"], dry_run=dry_run)
        if created:
            diag.fixed(msg)
        else:
            print(f"  SKIP:  {msg}")

    # Step 2: Check if vendor directory is populated
    vendor_ok, _ = check_vendor_dir()
    if not vendor_ok:
        diag.error(
            "vendor/ directory is missing! You must run:\n"
            "         ./bootstrap/reindeer --third-party-dir shim/third-party/rust vendor"
        )

    # Step 3: Remind to regenerate BUCK.reindeer
    if diag.fixes_applied:
        print(
            f"\n  NOTE:  After fixing, regenerate build rules with:\n"
            f"         ./bootstrap/reindeer --third-party-dir shim/third-party/rust buckify"
        )

    return diag


def bootstrap(dry_run=False):
    """Run the full bootstrap process: vendor + buckify."""
    diag = DiagnosticResult()

    print("\n=== Running full Rust third-party bootstrap ===\n")

    if dry_run:
        print("  DRY RUN: Would run reindeer vendor")
        print("  DRY RUN: Would run reindeer buckify")
        return diag

    # Step 1: Vendor all crate sources
    print("Step 1/2: Vendoring crate sources...")
    ok, msg = run_reindeer("vendor")
    if ok:
        diag.fixed(msg)
    else:
        diag.error(msg)
        print("\n  Cannot proceed to buckify without successful vendoring.")
        return diag

    # Step 2: Generate BUCK.reindeer
    print("\nStep 2/2: Generating Buck2 build rules...")
    ok, msg = run_reindeer("buckify")
    if ok:
        diag.fixed(msg)
    else:
        diag.error(msg)

    if diag.ok:
        print("\n  Bootstrap complete! You can now build with Buck2.")
    else:
        print("\n  Bootstrap encountered errors. See above for details.")

    return diag


def main():
    args = sys.argv[1:]

    if not args:
        # No arguments: run full diagnostic
        diag = DiagnosticResult()
        diagnose(diag)
        if not diag.ok:
            print("\n=== Recommended fix ===\n")
            print("  Run the full bootstrap to vendor sources and generate build rules:\n")
            print("    ./bootstrap/reindeer --third-party-dir shim/third-party/rust vendor")
            print("    ./bootstrap/reindeer --third-party-dir shim/third-party/rust buckify")
            print()
            print("  Or use this script:")
            print("    python3 tools/rust_auto_fix.py --bootstrap")
        sys.exit(0 if diag.ok else 1)

    if args[0] == "--bootstrap":
        diag = bootstrap(dry_run="--check" in args)
        sys.exit(0 if diag.ok else 1)

    if args[0] == "--check":
        diag = DiagnosticResult()
        crate = args[1] if len(args) > 1 else None
        diagnose(diag, crate_name=crate)
        sys.exit(0 if diag.ok else 1)

    # Treat argument as a crate name
    crate_name = args[0]
    dry_run = "--check" in args
    diag = fix_crate(crate_name, dry_run=dry_run)

    if not diag.ok:
        print(f"\n  Some issues could not be auto-fixed. See errors above.")
        sys.exit(1)
    else:
        print(f"\n  Done. Remember to run reindeer buckify to regenerate BUCK.reindeer!")
        sys.exit(0)


if __name__ == "__main__":
    main()
