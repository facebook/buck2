{
  description = "A flake for hacking on and building buck2";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
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
        ]);
        packages = [ pkgs.cargo-bloat my-rust-bin ];
        shellHook = ''
          export BUCK2_BUILD_PROTOC=${pkgs.protobuf}/bin/protoc
          export BUCK2_BUILD_PROTOC_INCLUDE=${pkgs.protobuf}/include
        '';
      };
    });
}
