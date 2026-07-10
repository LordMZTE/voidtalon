{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }: utils.lib.eachSystem ["x86_64-linux" "aarch64-linux"] (system:
    let
      pkgs = (import nixpkgs { inherit system; });
      package = pkgs.haskellPackages.callCabal2nix "voidtalon" ./. { };
    in
    {
      devShells.default = pkgs.haskellPackages.shellFor {
        packages = hpkgs: [ package ];
        nativeBuildInputs = with pkgs.haskellPackages; [ ghc haskell-language-server cabal-install ];
        withHoogle = true;
      };
      packages.default = package;
    });
}

