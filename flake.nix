{
  description = "Interpreter Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux"; 
      pkgs = import nixpkgs {
        inherit system;
      };
      
      # Haskell tools for GHC 9.10
      haskellPkgs = pkgs.haskell.packages.ghc910;
      ghcWithPkgs = haskellPkgs.ghcWithPackages (ps: [
        ps.pretty-show
      ]);
      
      # Python 2.7 bundled with pip
      pythonWithPip = pkgs.python312.withPackages (ps: [ 
        ps.pip
        ps.typing 
      ]);
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        packages = [ 
          # Python tools
          pythonWithPip
          pkgs.pyright 

          # Haskell tools
          ghcWithPkgs
          pkgs.cabal-install
          haskellPkgs.haskell-language-server

          # Pascal
          pkgs.fpc
        ];

        shellHook = ''
          echo "Environment loaded: Python 3.12 (with pip) & Haskell (GHC 9.10)"
          python --version
          pip --version
        '';
      };
    };
}
