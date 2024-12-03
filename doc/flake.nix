 {
  description = "Build and launch an iHaskell Juypter environment with ∆Q included: `nix run`";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    ihaskell =  {
      url = "github:IHaskell/IHaskell";
      flake = false;
    };
    # DeltaQ - to be modified to release version when made public
    dq = {
      url = "git+ssh://git@github.com/DeltaQ-SD/dq-revamp";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter, ihaskell, dq}:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "aarch64-darwin"] (system:
      let
        pkgs' = nixpkgs.legacyPackages.${system};
        ghcVersion = "ghc96";

        # bring appropriate iHaskell overlay into scope
        overlayIHaskell = let
          known = { "ghc96" = "${ihaskell}/nix/overlay-9.6.nix";
                    "ghc98" = "${ihaskell}/nix/overlay-9.8.nix";};
        in import known.${ghcVersion};

        # add the libraries needed here
        overlayHaskellPackages = sel: sup: {
          nix-filter = import nix-filter;
          haskell = sup.haskell // {
            packages = sup.haskell.packages // {
              "${ghcVersion}" = sup.haskell.packages."${ghcVersion}".override {
                overrides = self: super: {
                  # specific to this build
                  probability-polynomial = super.callCabal2nix "probability-polynomial" "${dq}/lib/probability-polynomial" {};
                  deltaq = super.callCabal2nix "deltaq" "${dq}/lib/deltaq" {inherit (self) probability-polynomial;};
                };
              };
            };
          };
        };

        # build the appropriate iHaskell environment, including the ability to produce PDFs
        overlayJupyter= self: super: let
          jupyterlab = self.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
          mkMyJupyter = eeb:  super.pkgs.callPackage "${ihaskell}/nix/release.nix" { compiler = ghcVersion; }{
            extraEnvironmentBinaries = eeb;
            # packages from the (enhanced) haskell packages to be available in the
            # ihaskell jupyter environment
            packages = hp: [
              # 'standard' ones (from cabal)
              hp.Chart
              hp.HaTeX
              hp.diagrams
              hp.here
              hp.ihaskell-charts
              hp.ihaskell-diagrams
              hp.ihaskell-graphviz
              hp.ihaskell-hatex
              hp.pretty-hex
              hp.text
              # 'local' ones (defined above)
              hp.deltaq
              hp.probability-polynomial
            ];
            # packages from the o/s needed for the ihaskell/jupyter environment
            systemPackages = sp: [
              (sp.texlive.combine {
                inherit (sp.texlive)
                  scheme-medium
                  adjustbox
                  collectbox
                  environ
                  enumitem
                  pdfcol
                  tcolorbox
                  titling
                  ucs
                  upquote
                ;
              })
              sp.inkscape
              sp.pandoc
              sp.graphviz
            ];
          };
        in {
          ihaskell-dq = mkMyJupyter [ jupyterlab ];
        };

        pkgs  = import nixpkgs {inherit system; overlays = [overlayIHaskell overlayHaskellPackages overlayJupyter];};

        runme = pkgs.writeShellApplication {
          name = "run-ihaskell-dq";
          text =  ''
          if ! test -e ./iHaskell-Notebooks
          then
            echo You need to create a ./iHaskell-Notebooks "(either as directory or symbolic link to one)" first
            exit 1
          fi
          echo You may need port forwarding for the port used below. ssh -L nnnn:127.0.0.1:nnnn
              ${self.packages.${system}.default}/bin/jupyter-lab --no-browser --ip 0.0.0.0 --notebook-dir ./iHaskell-Notebooks "$@"
          '';
        };
      in {
        packages.default = pkgs.ihaskell-dq;

        apps.default = {
          type = "app";
          program = "${runme}/bin/run-ihaskell-dq";
        };
      });
}
