
# This is just a small shell that allows you to easily build this package with cabal.
# You can use it like the following:
#
# $ nix-shell
# $ # in the nix-shell...
# $ cabal build

{...}:

let
  nixpkgs-unstable-rev = "8fcb549a66d1996c88a63fb9b74a28e4663923c9";

  nixpkgs-unstable-src = builtins.fetchTarball {
    # commit hash nixpkgs-unstable as of 2022-08-17
    url =
      "https://github.com/NixOS/nixpkgs/archive/${nixpkgs-unstable-rev}.tar.gz";
    # obtained with 'nix-prefetch-url --unpack <url>'
    sha256 = "0s53hg0bkdfi0z2z2pzqpbsd7wgh0y12xh5lwvj8dwgdms5nqxx5";
  };

  nixpkgs = import nixpkgs-unstable-src {};

  myHaskPkgs = nixpkgs.haskell.packages.ghc924.override {
    overrides = hfinal: hprev: {
      # pg-transact's tests don't pass when building with Nix.
      pg-transact = nixpkgs.haskell.lib.dontCheck (nixpkgs.haskell.lib.markUnbroken hprev.pg-transact);
    };
  };

in

with nixpkgs;

myHaskPkgs.developPackage {
  # TODO: We should also filter out the dist-newstyle/ directory.
  root = lib.cleanSource ./.;
  name = "pg-transact-cluster";
  modifier =
    haskell.lib.compose.overrideCabal (oldAttrs: {
      extraLibraries = oldAttrs.extraLibraries or [] ++ [
        myHaskPkgs.haskell-language-server
      ];
    });
}
