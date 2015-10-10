{ mkDerivation, base, containers, mtl, ref-fd, stdenv, transformers
}:
mkDerivation {
  pname = "persistent-refs";
  version = "0.4";
  src = ./.;
  buildDepends = [ base containers mtl ref-fd transformers ];
  homepage = "https://github.com/acfoltzer/persistent-refs";
  description = "Haskell references backed by an IntMap for persistence and reversibility";
  license = stdenv.lib.licenses.bsd3;
}
