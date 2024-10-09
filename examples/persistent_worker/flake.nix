{
  description = "Buck2 Remote Persistent Worker Example";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
  flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [];
      config = {};
    };
    python = (pkgs.python3.override {
      packageOverrides = self: super: {
        protobuf = self.protobuf5;
      };
    }).withPackages (ps: with ps; [
      ipython
      grpcio
      grpcio-tools
    ]);
  in
  {
    devShells = {
      default = pkgs.mkShellNoCC {
        packages = [ python ];
        NIX_PYTHON = "${python}/bin/python";
        shellHook = ''
          cat >toolchains/.buckconfig.nix <<EOF
          [nix]
          python = ${python}/bin/python
          EOF
        '';
      };
    };

    apps = {
      dockerBuild = {
        type = "app";
        program = "${self.packages.${system}.dockerImage}";
      };
    };

    packages = {
      dockerImage = pkgs.dockerTools.streamNixShellImage {
        name = "nix-build";
        drv = pkgs.mkShell.override { stdenv = pkgs.stdenvNoCC; }
          {
            PATH = pkgs.lib.makeBinPath [ pkgs.bash pkgs.coreutils ];
            nativeBuildInputs = [ python ];
          };
        tag = "latest";
      };
    };
  });
}
