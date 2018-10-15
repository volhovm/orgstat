with import <nixpkgs> { };
haskell.lib.buildStackProject {
   ghc = haskell.packages.ghc843.ghc;
   name = "orgstat";
   # firefox for xdg-open
   buildInputs = [ zlib git openssh gnupg gnupg1compat xdg_utils firefox gmp ];
   LANG = "en_US.UTF-8";
}

