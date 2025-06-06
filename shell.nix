let
  pkgs = import ./packages.nix;
  project = import ./default.nix;
in
project.shellFor {
  exactDeps = true;
  buildInputs = [
    pkgs.ghcid
    # pkgs.libpq
    # pkgs.haskellPackages.libpq
    # pkgs.haskellPackages.postgresql-libpq
    pkgs.postgresql
    pkgs.stack
  ];
  tools = {
    cabal = "latest";
    haskell-language-server = "latest";
  };
  withHoogle = false;
}
