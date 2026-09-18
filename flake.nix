{
  description = "tumbel.el — an interactive Tumblr client for Emacs";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Release builds of older Emacsen; nixpkgs only carries the current one.
    # Its nixpkgs is deliberately not made to follow ours: the binary cache
    # below only holds builds against its own pin.
    nix-emacs-ci.url = "github:purcell/nix-emacs-ci";
  };

  nixConfig = {
    extra-substituters = "https://emacs-ci.cachix.org";
    extra-trusted-public-keys = "emacs-ci.cachix.org-1:B5FVOrxhXXrOL0S+tQ7USrhjMT5iOPH+QN9q0NItom4=";
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
        {
          pkgs,
          lib,
          system,
          inputs',
          ...
        }:
        let
          emacs = pkgs.emacs-nox;
          epkgs = pkgs.emacsPackagesFor emacs;

          # Older releases the tests also run under, as nix-emacs-ci attribute
          # names: the last point release of each major version from the
          # Package-Requires floor in tumbel.el up, those being what people
          # on an older major actually run. Point releases add no API, and
          # package-lint holds the code to the floor itself.
          olderEmacsen = [
            "emacs-29-4"
            "emacs-30-2"
          ];

          # Libraries tumbel.el needs at runtime. Keep in sync with the
          # Package-Requires header in tumbel.el.
          runtimeDeps = e: [ e.plz ];

          # Tooling for the devshell and checks; never shipped with the package.
          # evil and evil-snipe are here rather than in runtimeDeps because
          # tumbel-evil.el only acts once the user has loaded them; the tests
          # need the real things to check key precedence.
          testDeps = e: [
            e.evil
            e.evil-snipe
          ];

          # Kept apart from testDeps so the older Emacsen, which only run the
          # tests, don't depend on the linters still supporting them.
          lintDeps = e: [ e.elisp-lint ];

          withPackages =
            emacs: deps: (pkgs.emacsPackagesFor emacs).emacsWithPackages (e: runtimeDeps e ++ deps e);

          emacsDev = withPackages emacs (e: testDeps e ++ lintDeps e);

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
            name: emacsEnv: target:
            pkgs.runCommand "tumbel-${name}"
              {
                nativeBuildInputs = [
                  emacsEnv
                  pkgs.just
                ];
              }
              ''
                cp -r ${src} src && chmod -R u+w src && cd src
                just ${target}
                touch $out
              '';

          # `just test` byte-compiles with warnings as errors first, so each
          # of these covers both the compiler and the suites of that release.
          # nix-emacs-ci has no binary cache for aarch64-linux, where every
          # run would compile each Emacs from source; nothing here depends on
          # the architecture, so the other systems cover it.
          olderEmacsChecks = lib.optionalAttrs (system != "aarch64-linux") (
            lib.genAttrs' olderEmacsen (
              name:
              lib.nameValuePair "test-${name}" (
                mkMakeCheck "test-${name}" (withPackages inputs'.nix-emacs-ci.packages.${name} testDeps) "test"
              )
            )
          );
        in
        {
          packages.default = tumbel;

          checks = {
            build = tumbel;
            test = mkMakeCheck "test" emacsDev "test";
            lint = mkMakeCheck "lint" emacsDev "lint";
          }
          // olderEmacsChecks;

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
