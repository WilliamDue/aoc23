{ pkgs ? import <nixos-unstable> { config = { allowUnfree = true; }; } }:
pkgs.mkShell {
  # nativeBuildInputs is usually what you want -- tools you need to run
  nativeBuildInputs = with pkgs; [
    dotnet-sdk_7
    fsautocomplete
    ghc
    haskell-language-server
  ];
}
