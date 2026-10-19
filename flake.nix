{
  description = "HTTP message values, HTTP/1 wire codecs, and HTTP/2/3 framing and header compression.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    paredit-cli = {
      url = "github:nerima-lisp/paredit-cli/v1.6.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      cl-weave,
      paredit-cli,
      ...
    }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];
      forEachSystem =
        function:
        nixpkgs.lib.genAttrs systems (system: function system (import nixpkgs { inherit system; }));
      testProgram =
        system: pkgs:
        pkgs.writeShellApplication {
          name = "cl-http-message-kit-test";
          runtimeInputs = [
            cl-weave.packages.${system}.default
            pkgs.coreutils
          ];
          text = ''
            export HOME="$TMPDIR"
            export CL_SOURCE_REGISTRY="$PWD//"
            exec timeout --signal=TERM --kill-after=10s 120s \
              cl-weave run \
              --load "$PWD/cl-http-message-kit.asd" \
              cl-http-message-kit/test \
              --reporter spec \
              --fail-with-no-tests \
              --test-timeout-ms 30000 \
              --max-workers 1
          '';
        };
      testSource = builtins.path {
        path = ./.;
        name = "cl-http-message-kit-test-source";
      };
    in
    {
      formatter = forEachSystem (system: pkgs: pkgs.nixfmt-tree);

      # The source tree, installed where an ASDF source registry expects to
      # find it. A consumer adds "${cl-http-message-kit}/share/common-lisp/source//"
      # to CL_SOURCE_REGISTRY; there is nothing to compile ahead of time.
      packages = forEachSystem (
        system: pkgs: {
          default = pkgs.stdenvNoCC.mkDerivation {
            pname = "cl-http-message-kit";
            version = "1.0.0";
            src = builtins.path {
              path = ./.;
              name = "cl-http-message-kit-source";
            };
            dontBuild = true;
            installPhase = ''
              runHook preInstall
              target="$out/share/common-lisp/source/cl-http-message-kit"
              mkdir -p "$target"
              cp -r cl-http-message-kit.asd src t "$target"/
              runHook postInstall
            '';
            meta = {
              description = "HTTP message values, HTTP/1 wire codecs, and HTTP/2/3 framing and header compression";
              license = pkgs.lib.licenses.mit;
            };
          };
        }
      );

      checks = forEachSystem (
        system: pkgs:
        let
          test = testProgram system pkgs;
        in
        {
          test =
            pkgs.runCommand "cl-http-message-kit-test-check"
              {
                src = testSource;
              }
              ''
                set -e
                cp -r "$src"/. .
                chmod -R u+w .
                ${test}/bin/cl-http-message-kit-test
                mkdir -p "$out"
              '';
        }
      );

      devShells = forEachSystem (
        system: pkgs: {
          default = pkgs.mkShell {
            packages = [
              cl-weave.packages.${system}.default
              paredit-cli.packages.${system}.default
              pkgs.sbcl
              pkgs.coreutils
              pkgs.perl
            ];
          };
        }
      );

      apps = forEachSystem (
        system: pkgs:
        let
          clWeave = cl-weave.packages.${system}.default;
          test = testProgram system pkgs;
          coverage = pkgs.writeShellApplication {
            name = "cl-http-message-kit-coverage";
            runtimeInputs = [
              clWeave
              pkgs.coreutils
            ];
            text = ''
              export CL_SOURCE_REGISTRY="$PWD//"
              exec timeout --signal=TERM --kill-after=10s 300s \
                cl-weave run \
                --load "$PWD/cl-http-message-kit.asd" \
                cl-http-message-kit/test \
                --reporter spec \
                --fail-with-no-tests \
                --test-timeout-ms 30000 \
                --max-workers 1 \
                --coverage \
                --coverage-output cl-http-message-kit.coverage \
                --coverage-report-directory cl-http-message-kit-coverage-report \
                --coverage-system cl-http-message-kit \
                --coverage-exclude src/package.lisp \
                --coverage-exclude src/conditions.lisp \
                --coverage-exclude src/model-declarations.lisp \
                --coverage-exclude src/defaults.lisp \
                --coverage-exclude src/status-registry.lisp \
                --coverage-exclude src/header-data.lisp \
                --coverage-exclude src/hpack-data.lisp \
                --coverage-min-expression 100 \
                --coverage-min-branch 100
            '';
          };
        in
        {
          default = {
            type = "app";
            program = "${test}/bin/cl-http-message-kit-test";
            meta = {
              description = "Run the cl-http-message-kit test suite.";
            };
          };
          test = {
            type = "app";
            program = "${test}/bin/cl-http-message-kit-test";
            meta = {
              description = "Run the cl-http-message-kit test suite.";
            };
          };
          coverage = {
            type = "app";
            program = "${coverage}/bin/cl-http-message-kit-coverage";
            meta = {
              description = "Run cl-http-message-kit with coverage gates.";
            };
          };
        }
      );
    };
}
