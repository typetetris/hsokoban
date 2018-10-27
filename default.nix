{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, filepath, linear, protolude
      , sdl2, sdl2-image, stdenv, text, vector, megaparsec, sdl2-ttf
      }:
      mkDerivation {
        pname = "hsokoban";
        version = "0.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base containers filepath linear protolude sdl2 sdl2-image text
          vector megaparsec sdl2-ttf
        ];
        license = stdenv.lib.licenses.unfree;
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages1 = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  haskellPackages = with pkgs.haskell.lib; haskellPackages1.override {
                      overrides = self: super: {
                        sdl2 = dontCheck super.sdl2;
                        sdl2-image = dontCheck super.sdl2-image;
                      };
                    };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then (pkgs.haskell.lib.addBuildDepends drv (with haskellPackages; [ghcid hlint cabal-install])).env else drv
