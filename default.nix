{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = with pkgs.buildPackages; [
      ghc
      haskell-language-server
      haskellPackages.hlint
      haskellPackages.fourmolu
    ];
}
