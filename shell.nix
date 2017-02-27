with import <nixpkgs> { };
haskell.lib.buildStackProject {
   ghc = haskell.packages.ghc802.ghc;
   name = "orgstat";
   buildInputs = [ zlib git openssh gnupg gnupg1compat xdg_utils firefox ];
   LANG = "en_US.UTF-8";
}

