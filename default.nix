let
  pkgs = import ./packages.nix;
in pkgs.haskell-nix.cabalProject {
  compiler-nix-name = "ghc98";
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "database-generic";
    src = ./.;
  };
}
