{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    ipso.url = "github:LightAndLight/ipso?tag=v0.5";
  };
  outputs = { self, nixpkgs, flake-utils, ipso }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
        ghcVersion = "927";
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.packages."ghc${ghcVersion}".ghc
            cabal-install
            (haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; })
            
            haskell.packages."ghc${ghcVersion}".hakyll
            zlib
            
            ipso.defaultPackage.${system}
          ];
        };
      }
    );
}
