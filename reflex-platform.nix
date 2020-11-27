let
  initialNixpkgs = import <nixpkgs> {};

  sources = {
     reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "097e0ef01539f1ba23bd6a4e2aaedc7d2b114a26";
      sha256 = "sha256-UfD12E6nCuKgNQsqXpxha769YzwN6FXLs2E7tPDmKCU";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform
