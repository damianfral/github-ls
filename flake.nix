{
  description = "CLI tool to list your repositories";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
    nix-filter.url = "github:numtide/nix-filter";
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
  };

  outputs = { self, nixpkgs, flake-utils, safe-coloured-text, nix-filter, ... }:

    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
          safe-coloured-text.overlays.${system}
          nix-filter.overlays.default
        ];
      };
    in

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = pkgsFor system;
        filteredSrc =
          pkgs.nix-filter {
            root = ./.;
            include = [
              "src/"
              "test/"
              "package.yaml"
              "LICENSE"
            ];
          };
      in
      rec {
        packages = {
          github-ls =
            pkgs.haskell.lib.justStaticExecutables (
              pkgs.haskellPackages.github-ls.overrideAttrs
                (oldAttrs: {
                  nativeBuildInputs = oldAttrs.nativeBuildInputs
                    ++ [ pkgs.makeWrapper ];
                  postInstall = (oldAttrs.postInstall or "") + ''
                    wrapProgram $out/bin/github-ls \
                      --suffix PATH : ${pkgs.lib.makeBinPath [pkgs.github-cli]}
                  '';
                }
                ));
        };

        defaultPackage = packages.github-ls;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ packages.github-ls ];
          buildInputs = with pkgs; with pkgs.haskellPackages; [
            haskell-language-server
            cabal-install
            ghcid
            hpack
            hlint
            yamlfix
          ];
        };

        overlays = final: prev: with final.haskell.lib; {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
              (self: super: {
                autodocodec-yaml = unmarkBroken super.autodocodec-yaml;
                github-ls = self.generateOptparseApplicativeCompletions
                  [ "github-ls" ]
                  (self.callCabal2nix "github-ls" filteredSrc { });
                sydtest = unmarkBroken (dontCheck super.sydtest);
              }
              );
          });
        };
      });
}

