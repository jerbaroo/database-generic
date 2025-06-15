let
  pkgs = import ./packages.nix;
in
pkgs.haskell-nix.cabalProject {
  compiler-nix-name = "ghc910";
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "database-generic";
    src = ./.;
  };
}
