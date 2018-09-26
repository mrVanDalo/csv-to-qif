{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, Cabal, explicit-exception, hspec
      , parsec, QuickCheck, regex-tdfa, split, spreadsheet, stdenv
      }:
      mkDerivation {
        pname = "csv-to-qif";
        version = "0.3.3";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base explicit-exception parsec regex-tdfa split spreadsheet
        ];
        testHaskellDepends = [
          base Cabal explicit-exception hspec parsec QuickCheck regex-tdfa
          split spreadsheet
        ];
        homepage = "http://mrvandalo.github.io/csv-to-qif/";
        description = "A small program that will read csv files and create qif files";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
