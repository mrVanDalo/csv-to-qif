{ pkgs ?  import <nixpkgs> {} }:
pkgs.mkShell {

  buildInputs = with pkgs; [
    cabal2nix
  ];

  shellHook = ''
    HISTFILE=${toString ./.}/.history
  '';
}
