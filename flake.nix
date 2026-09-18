{
  description = "tumbel.el — an interactive Tumblr client for Emacs";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem =
        { pkgs, lib, ... }:
        let
          emacs = pkgs.emacs-nox;
          epkgs = pkgs.emacsPackagesFor emacs;

          # Libraries tumbel.el needs at runtime. Keep in sync with the
          # Package-Requires header in tumbel.el.
          runtimeDeps = e: [ e.plz ];

          # Tooling for the devshell and checks; never shipped with the package.
          # evil and evil-snipe are here rather than in runtimeDeps because
          # tumbel-evil.el only acts once the user has loaded them; the tests
          # need the real things to check key precedence.
          devDeps = e: [
            e.elisp-lint
            e.evil
            e.evil-snipe
          ];

          emacsDev = epkgs.emacsWithPackages (e: runtimeDeps e ++ devDeps e);

          # Only the files the build and tests actually read, so edits to
          # docs or Nix files don't invalidate the derivations.
          src = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./justfile
              ./test/fixtures
              (lib.fileset.fileFilter (f: f.hasExt "el") ./.)
            ];
          };

          tumbel = epkgs.trivialBuild {
            pname = "tumbel";
            version = "0.1.0";
            inherit src;
            packageRequires = runtimeDeps epkgs;
            meta = {
              description = "Interactive Tumblr client for Emacs";
              license = lib.licenses.gpl3Plus;
              platforms = lib.platforms.all;
            };
          };

          # The justfile is the single definition of how to test and lint;
          # checks just run it against the store copy of the sources.
          mkMakeCheck =
            target:
            pkgs.runCommand "tumbel-${target}"
              {
                nativeBuildInputs = [
                  emacsDev
                  pkgs.just
                ];
              }
              ''
                cp -r ${src} src && chmod -R u+w src && cd src
                just ${target}
                touch $out
              '';
        in
        {
          packages.default = tumbel;

          checks = {
            build = tumbel;
            test = mkMakeCheck "test";
            lint = mkMakeCheck "lint";
          };

          formatter = pkgs.nixfmt-tree;

          devShells.default = pkgs.mkShell {
            packages = [
              emacsDev
              pkgs.just
              pkgs.curl
              pkgs.nixfmt
              pkgs.deadnix
              pkgs.statix
            ];
          };
        };
    };
}
