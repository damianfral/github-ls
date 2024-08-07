{
  description = "CLI tool to list your repositories";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.inputs = {
      nixpkgs.follows = "nixpkgs";
      pre-commit-hooks.follows = "pre-commit-hooks";
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , nix-filter
    , pre-commit-hooks
    , ...
    } @ inputs:

    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default ];
      };
    in
    {
      overlays.default = final: prev:
        let
          filteredSrc =
            nix-filter.lib {
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
          github-ls = final.haskell.lib.justStaticExecutables haskellPackages.github-ls;
          haskellPackages = prev.haskellPackages.override
            (old: {
              overrides = final.lib.composeExtensions
                (old.overrides or (_: _: { }))
                (self: super: {
                  github-ls = self.generateOptparseApplicativeCompletions
                    [ "github-ls" ]
                    ((self.callCabal2nix "github-ls" filteredSrc { }).overrideAttrs
                      (oldAttrs: {
                        nativeBuildInputs = oldAttrs.nativeBuildInputs
                        ++ [ final.makeWrapper ];
                        postInstall = (oldAttrs.postInstall or "") + ''
                          wrapProgram $out/bin/github-ls \
                            --suffix PATH : ${final.lib.makeBinPath [final.github-cli] }
                        '';
                      }
                      )
                    );
                }
                );
            });
        }
      ;
    } //
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = pkgsFor system;
      precommitCheck = pre-commit-hooks.lib.${system}.run {
        src = ./.;
        hooks = {
          actionlint.enable = true;
          hlint.enable = true;
          hpack.enable = true;
          markdownlint.enable = true;
          nil.enable = true;
          nixpkgs-fmt.enable = true;
          ormolu.enable = true;
        };
      };
    in
    rec {
      packages = {
        inherit (pkgs) github-ls;
        default = packages.github-ls;
      };

      apps = {
        github-ls = flake-utils.lib.mkApp { drv = packages.github-ls; };
        default = apps.github-ls;
      };

      checks = {
        pre-commit-check = precommitCheck;
        weeder-check = inputs.weeder-nix.lib.${system}.makeWeederCheck {
          haskellPackages = pkgs.haskellPackages;
          packages = [ "github-ls" ];
          reportOnly = true;
        };
      };

      devShells.default = pkgs.haskellPackages.shellFor {
        packages = p: [ packages.github-ls ];
        buildInputs = with pkgs; with pkgs.haskellPackages; [
          actionlint
          cabal-install
          ghcid
          haskell-language-server
          hlint
          nil
          nixpkgs-fmt
          ormolu
          statix
        ];
        inherit (precommitCheck) shellHook;
      };
    }
    );
  nixConfig = {
    extra-substituters = "https://opensource.cachix.org";
    extra-trusted-public-keys = "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc=";
  };
}

