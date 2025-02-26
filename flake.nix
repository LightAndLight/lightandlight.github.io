{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    ipso.url = "github:LightAndLight/ipso?tag=v0.5";
  };
  outputs = { self, nixpkgs, flake-utils, ipso }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.ghc
            cabal-install
            haskell-language-server
            
            haskellPackages.hakyll
            zlib
            
            ipso.defaultPackage.${system}
          ];
        };
      }
    );
}
