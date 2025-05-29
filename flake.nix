# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.
{
  description = "A flake for hacking on and building buck2";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ (import rust-overlay) ];
      };

    rust-version = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain;
    my-rust-bin = rust-version.override {
      extensions = [ "rust-analyzer" "rust-src" ];
    };

    in {
      devShells.default = pkgs.mkShell {
        buildInputs = pkgs.lib.optionals pkgs.stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
          CoreFoundation
          CoreServices
          IOKit
          Security
        ]) ++ [
          # NOTE (aseipp): needed on aarch64-linux, so that the linker can
          # properly find libatomic.so, but harmless elsewhere
          pkgs.stdenv.cc.cc
        ];
        packages = [ pkgs.cargo-bloat my-rust-bin pkgs.mold-wrapped pkgs.reindeer pkgs.lld_16 pkgs.clang_16 ];
        shellHook =
          ''
            export BUCK2_BUILD_PROTOC=${pkgs.protobuf}/bin/protoc
            export BUCK2_BUILD_PROTOC_INCLUDE=${pkgs.protobuf}/include
          ''
          # enable mold for linux users, for more tolerable link times
          # we have to specify tokio_unstable in the RUSTFLAGS here since they override
          # .cargo/config.toml that is the reasonable place to specify it
          + pkgs.lib.optionalString pkgs.stdenv.isLinux ''
            export RUSTFLAGS="-C linker=clang -C link-arg=-fuse-ld=mold --cfg=tokio_unstable $RUSTFLAGS"
          '';
      };
    });
}
