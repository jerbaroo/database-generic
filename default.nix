let
  pkgs = import ./packages.nix;
in
pkgs.haskell-nix.cabalProject {
  compiler-nix-name = "ghc984";
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "database-generic";
    src = ./.;
  };
}
