{
  description = "CLI tool to list your repositories";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
  };

  outputs = { self, nixpkgs, flake-utils, safe-coloured-text, ... }@inputs:

    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
          safe-coloured-text.overlays.${system}
        ];
      };
    in

    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = pkgsFor system;
        in
        rec {
          packages = {
            github-ls = pkgs.haskellPackages.github-ls.overrideAttrs (oldAttrs: {
              nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.makeWrapper ];
              postInstall = ''
                wrapProgram $out/bin/github-ls \
                  --suffix PATH : ${pkgs.lib.makeBinPath [ pkgs.github-cli ]}
              '';
            }
            );
          };

          defaultPackage = pkgs.haskell.lib.justStaticExecutables
            packages.github-ls;

          devShells.default = pkgs.haskellPackages.shellFor {
            packages = p: [ packages.github-ls ];
            buildInputs = with pkgs; with pkgs.haskellPackages; [
              haskell-language-server
              cabal-install
              ghcid
              hpack
              yamlfix
            ];
          };

          overlays = final: prev: {
            haskellPackages = prev.haskellPackages.override (old: {
              overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
                (self: super: {
                  autodocodec-yaml = final.haskell.lib.unmarkBroken super.autodocodec-yaml;
                  github-ls = self.callCabal2nix "github-ls" ./. { };
                  sydtest = with final.haskell.lib; unmarkBroken (dontCheck super.sydtest);
                }
                );
            });
          };
        });
}

