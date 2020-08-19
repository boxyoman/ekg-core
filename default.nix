{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, ghc-prim, stdenv, text
      , unordered-containers
      }:
      mkDerivation {
        pname = "ekg-core";
        version = "0.1.1.7";
        src = ./.;
        libraryHaskellDepends = [
          base containers ghc-prim text unordered-containers
        ];
        benchmarkHaskellDepends = [ base ];
        homepage = "https://github.com/tibbe/ekg-core";
        description = "Tracking of system metrics";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
