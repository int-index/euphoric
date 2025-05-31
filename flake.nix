{
  description = "euphoric - a parser generator";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      ghc = "ghc912";
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages =
        pkgs.haskell.packages.${ghc}.extend(hself: hsuper: {
          euphoric = haskellPackages.callCabal2nix "euphoric" "${self}" {};
        });
    in
    {
      defaultPackage.${system} = haskellPackages.euphoric;
      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          (haskellPackages.ghcWithPackages(p: p.euphoric.getCabalDeps.libraryHaskellDepends))
          # haskellPackages.hie-bios
          # haskellPackages.haskell-language-server
          # haskellPackages.cabal-install
          pkgs.cabal-install
        ];
      };
    };
}
