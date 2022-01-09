{ mkDerivation, aeson, aeson-pretty, base, bytestring, Cabal
, containers, deepseq, doctest, flow
, hashable, intern, lens, megaparsec, monad-mock
, mtl, optparse-generic, QuickCheck, quickcheck-instances
, semigroups, smallcheck, lib, system-filepath, tasty
, tasty-html, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, template-haskell, text, transformers, turtle
, unordered-containers, versions
}:
mkDerivation {
  pname = "language-ninja";
  version = "0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal ];
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq flow hashable intern lens
    megaparsec mtl QuickCheck semigroups smallcheck system-filepath
    text transformers unordered-containers versions
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base flow lens mtl optparse-generic text
    transformers
  ];
  testHaskellDepends = [
    aeson base bytestring containers doctest flow
    hashable lens monad-mock mtl QuickCheck
    quickcheck-instances semigroups smallcheck system-filepath tasty
    tasty-html tasty-hunit tasty-quickcheck tasty-smallcheck
    template-haskell text transformers turtle unordered-containers
    versions
  ];
  homepage = "https://github.com/awakesecurity/language-ninja";
  description = "A library for dealing with the Ninja build language";
  license = lib.licenses.asl20;
}
