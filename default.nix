{ mkDerivation, base, containers, extra, filepath, linear
, megaparsec, pretty-simple, protolude, sdl2, sdl2-image, sdl2-ttf
, stdenv, text, vector
}:
mkDerivation {
  pname = "hsokoban";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base containers extra filepath linear megaparsec pretty-simple
    protolude sdl2 sdl2-image sdl2-ttf text vector
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
