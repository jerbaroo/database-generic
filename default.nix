let
  pkgs = import ./packages.nix;
in pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "database-generic";
    src = ./.;
  };
}
