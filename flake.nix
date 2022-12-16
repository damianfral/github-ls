{
  description = "CLI tool to list your repositories";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:

    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ ];
      };
    in

    flake-utils.lib.eachDefaultSystem
      (system:

        let
          pkgs = pkgsFor system;
          haskellPackages = pkgs.haskellPackages.extend
            (pkgs.haskell.lib.compose.packageSourceOverrides {
              github-ls = ./.;
            });
        in
        rec {
          packages = rec {
            github-ls = haskellPackages.github-ls;
          };

          defaultPackage = packages.github-ls;
          devShells.default = haskellPackages.shellFor
            {
              packages = p: [ packages.github-ls ];
              buildInputs = with pkgs; with haskellPackages; [
                haskell-language-server
                cabal-install
                ghcid
                implicit-hie
              ];
            };
        }
      );
}
