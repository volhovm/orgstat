with import <nixpkgs> { };
haskell.lib.buildStackProject {
   ghc = haskell.packages.ghc801.ghc;
   name = "orgstat";
   buildInputs = [ zlib git openssh gnupg gnupg1compat ];
   LANG = "en_US.UTF-8";
}

