{
  description = "Flake for Sockets and Pipes book";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
      rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [
          ];

          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
            fourmolu
          ];

          withHoogle = true;
        };
      }
    );
}
