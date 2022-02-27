{ pkgs ? import <nixpkgs> {}}:
let
  project = pkgs.haskellPackages.callPackage ./default.nix {};
in
pkgs.mkShell {
  name = project.pname;
  version = project.version;
  buildInputs = with pkgs; project.env.nativeBuildInputs ++ [
    ghc
    cabal-install
    ghcid
  ];
}
