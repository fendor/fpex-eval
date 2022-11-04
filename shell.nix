with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "fpex-eval";
  buildInputs = [
    gmp
    zlib
    ncurses

    haskellPackages.cabal-install
    haskell.compiler.ghc8107
  ];
  src = null;
  shellHook = ''
    export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib
    export PATH=$PATH:$HOME/.local/bin
  '';
}
