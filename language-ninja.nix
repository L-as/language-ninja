{ mkDerivation, aeson, aeson-pretty, base, bytestring, containers
, deepseq, flow, hashable, intern, lens, lib, megaparsec
, monad-mock, mtl, optparse-generic, QuickCheck
, quickcheck-instances, semigroups, smallcheck, system-filepath
, tasty, tasty-html, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, text, transformers, turtle
, unordered-containers, versions
}:
mkDerivation {
  pname = "language-ninja";
  version = "0.3.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq flow hashable intern lens
    megaparsec mtl QuickCheck semigroups smallcheck system-filepath
    text transformers unordered-containers versions
  ];
  testHaskellDepends = [
    aeson aeson-pretty base bytestring containers flow hashable lens
    monad-mock mtl optparse-generic QuickCheck quickcheck-instances
    semigroups smallcheck system-filepath tasty tasty-html tasty-hunit
    tasty-quickcheck tasty-smallcheck text transformers turtle
    unordered-containers versions
  ];
  homepage = "https://github.com/L-as/language-ninja";
  description = "A library for dealing with the Ninja build language";
  license = lib.licenses.asl20;
}
