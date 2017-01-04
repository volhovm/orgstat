with import <nixpkgs> { };
haskell.lib.buildStackProject {
   ghc = haskell.packages.ghc801.ghc;
   name = "orgstat";
   buildInputs = [ git openssh ];
   LANG = "en_US.UTF-8";
}

