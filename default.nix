{ reflex-platform ? ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "8f8d5efc7b844d6eb0b9d909beb3450c1db026dd";
    sha256 = "sha256-oYDetOqfnt/v43xhqb0VI8ulIrNSGby4cJNGRlr/m+g=";
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
    ghc   = ["reflex-indexed-db" ];
    ghcjs = ["reflex-indexed-db" ];
  };
})
