{ pkgs ? import <nixos> { } }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc902
    pkgs.stack
    pkgs.haskell-language-server
    pkgs.ormolu
    pkgs.haskellPackages.implicit-hie
  ];
}
