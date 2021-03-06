{ mkDerivation, aeson, base, bytestring, containers, directory
, filepath, hspec, http-media, insert-ordered-containers, lens, lib
, mtl, openapi3, optparse-applicative, prettyprinter, relude, tasty
, tasty-discover, tasty-golden, tasty-hspec, tasty-hunit, text
, unordered-containers, yaml
}:
mkDerivation {
  pname = "tie";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath http-media
    insert-ordered-containers lens mtl openapi3 prettyprinter relude
    text unordered-containers yaml
  ];
  executableHaskellDepends = [ base optparse-applicative relude ];
  testHaskellDepends = [
    aeson base bytestring containers filepath hspec
    insert-ordered-containers openapi3 prettyprinter relude tasty
    tasty-golden tasty-hspec tasty-hunit text yaml
  ];
  testToolDepends = [ tasty-discover ];
  description = "Tie allows generation of Haskell server stubs from OpenAPI (v 3.x) specifications.";
  license = lib.licenses.asl20;
}
