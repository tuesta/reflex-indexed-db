{ reflex-platform ? ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "9ebc4aa483e94ea52a20ce3641e72b43328d40ae";
    sha256 = "sha256-Wl0weU5PznbfRktKk7ZNOfx8RgjsRsbV12wx2rPa1rs=";
  })
 , system ? builtins.currentSystem
}:
(import reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  useWarp = true;
  withHoogle = true;

  packages = {
    reflex-indexed-db = ./.;
  };

  overrides = self : super : {
  };

  shells = {
    ghc   = ["reflex-indexed-db" "stylish-haskell"];
    ghcjs = ["reflex-indexed-db" ];
  };
})
