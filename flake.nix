{
  description = "A very basic flake";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays =
          [ (final: prev: { haskellPackages = prev.haskell.packages.ghc94; }) ];
      };
    in {
      devShells.x86_64-linux.default = pkgs.haskellPackages.shellFor {
        packages = p: [ (pkgs.haskellPackages.callCabal2nix "rlfu" ./. { }) ];
        buildInputs =
          [ pkgs.cabal-install pkgs.haskellPackages.haskell-language-server ];
      };
    };
}
