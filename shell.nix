{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, boxes, bytestring
      , colour, containers, data-default, diagrams-lib, diagrams-svg
      , directory, exceptions, filepath, formatting, hashable, hspec
      , HUnit, lens, linear, log-warper, mtl, optparse-simple
      , orgmode-parse, QuickCheck, quickcheck-text, stdenv, text, time
      , transformers, turtle, universum, yaml, generic-deriving
      }:
      mkDerivation {
        pname = "orgstat";
        version = "0.1.4";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson attoparsec base boxes bytestring colour containers
          data-default diagrams-lib diagrams-svg directory exceptions
          filepath formatting hashable lens linear log-warper mtl
          optparse-simple orgmode-parse text time turtle universum yaml generic-deriving
        ];
        executableHaskellDepends = [
          base bytestring directory exceptions filepath formatting log-warper
          optparse-simple universum
        ];
        testHaskellDepends = [
          base colour hspec HUnit lens QuickCheck quickcheck-text text time
          transformers universum
        ];
        homepage = "https://github.com/volhovM/orgstat";
        description = "Statistics visualizer for org-mode";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = pkgs.haskell.lib.doJailbreak (variant (haskellPackages.callPackage f {}));

in

  if pkgs.lib.inNixShell then drv.env else drv
