{
  description = "euphoric - a parser generator";
  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      ghc = "ghc924";
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages =
        pkgs.haskell.packages.${ghc}.extend(hself: hsuper: {
          euphoric = haskellPackages.callCabal2nix "euphoric" "${self}" {};
        });
    in
    {
      defaultPackage.${system} = haskellPackages.khutor;
      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          haskellPackages.hie-bios
          haskellPackages.haskell-language-server
          haskellPackages.cabal-install
        ];
      };
    };
}