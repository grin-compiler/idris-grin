let
  haskell = import ./nix/haskell.nix;
  llvm-overlay = self: super: {
    llvm-config = self.llvm_7;
  };
  extra-overlays = [ llvm-overlay ];
  pkgs = import ./nix/packages.nix (haskell // {
    overlays = haskell.overlays ++ extra-overlays;
  });
in
  pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    pkg-def-extras = [
      (hackage: {
        packages = {
          "system-posix-redirect" = hackage.system-posix-redirect."1.1.0.1".revisions.default;
        };
      })
    ];
    modules = [
      {
        packages.idris.configureFlags = [ "-fexeconly" ];
      }
    ];
  }
