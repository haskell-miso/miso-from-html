{

  inputs = {
    miso.url = "github:dmjio/miso";
  };

  outputs = { self, nixpkgs, flake-utils, ... } @ inputs:

    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in {
      devShell = inputs.miso.outputs.devShells.${system}.default;
      devShells.wasm = inputs.miso.outputs.devShells.${system}.wasm;
      packages = {
        default = pkgs.haskellPackages.callCabal2nix "miso-from-html" ./. {};
      };
    });

}

