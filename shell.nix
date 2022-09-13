{
  iohkNix ?
    import (
      builtins.fetchTarball "https://github.com/input-output-hk/iohk-nix/archive/edb2d2df2ebe42bbdf03a0711115cf6213c9d366.tar.gz"
    ) {},
  pkgs ?
    import ( # nixpkgs 22.05
      builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/ce6aa13369b667ac2542593170993504932eb836.tar.gz"
    ) {
      overlays = iohkNix.overlays.crypto;
      config = { allowBroken = true; };
    },
  haskellCompiler ? "ghc8107"
}:with pkgs;
let
  devTools = [
    git
    gnupg
    nodejs-14_x
    openssl
    pre-commit
  ];
  haskellEnv = with haskell.packages.${haskellCompiler}; [
    ghc
    cabal-install
    (fourmolu_0_6_0_0.override {
      Cabal = Cabal_3_6_3_0;
      ghc-lib-parser = ghc-lib-parser_9_2_2_20220307;
    })
    hlint
    haskell-language-server
  ];
  libraries = [
    libsodium-vrf
    lzma
    postgresql
    pkgconfig
    secp256k1
    socat
    zlib
  ] ++ lib.optional (!stdenv.isDarwin) systemd;
in mkShell {
  name = "agora-types";
  buildInputs = devTools ++ haskellEnv ++ libraries;
  GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

  shellHook = ''
    pre-commit install --install-hooks -t pre-commit -t commit-msg
  '';
}
