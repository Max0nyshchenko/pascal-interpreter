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
        # Required because Python 2.7 is EOL and marked insecure
        config.allowInsecurePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [
          "python-2.7.18.8" # Adjust version if nixpkgs updates the minor patch
        ];
      };
      
      # Haskell tools for GHC 9.10
      haskellPkgs = pkgs.haskell.packages.ghc910;
      
      # Python 2.7 bundled with pip
      pythonWithPip = pkgs.python27.withPackages (ps: [ 
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
          haskellPkgs.ghc
          pkgs.cabal-install
          haskellPkgs.haskell-language-server
        ];

        shellHook = ''
          echo "Environment loaded: Python 2.7 (with pip) & Haskell (GHC 9.10)"
          python --version
          pip --version
        '';
      };
    };
}
