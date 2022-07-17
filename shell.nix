with import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs;
    ref = "nixos-20.09";
    rev = "cd63096d6d887d689543a0b97743d28995bc9bc3";
}) {};
haskell.lib.buildStackProject {
   #ghc = haskell.packages.ghc8102.ghc;
   ghc = haskell.packages.ghc882.ghc;
   name = "orgstat";
   # firefox for xdg-open
   buildInputs = [ zlib git openssh gnupg gnupg1compat xdg_utils firefox gmp ];
   LANG = "en_US.UTF-8";
}
