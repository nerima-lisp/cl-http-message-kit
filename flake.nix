{
  description = "Validated HTTP message values: URIs, header fields, requests, responses, deadlines and conditions.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      cl-weave,
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
            version = "0.1.0";
            src = self;
            dontBuild = true;
            installPhase = ''
              runHook preInstall
              target="$out/share/common-lisp/source/cl-http-message-kit"
              mkdir -p "$target"
              cp -r cl-http-message-kit.asd src t "$target"/
              runHook postInstall
            '';
            meta = {
              description = "Validated HTTP message values: URIs, header fields, requests, responses, deadlines and conditions";
              license = pkgs.lib.licenses.mit;
            };
          };
        }
      );

      devShells = forEachSystem (
        system: pkgs: {
          default = pkgs.mkShell {
            packages = [
              cl-weave.packages.${system}.default
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
          sourceRegistry = "${clWeave}/share/common-lisp/source//";
          test = pkgs.writeShellApplication {
            name = "cl-http-message-kit-test";
            runtimeInputs = [
              pkgs.sbcl
              clWeave
            ];
            text = ''
              export CL_SOURCE_REGISTRY="$PWD//:${sourceRegistry}"
              sbcl --noinform --non-interactive \
                --eval '(require :asdf)' \
                --eval '(asdf:test-system "cl-http-message-kit")'
            '';
          };
        in
        {
          default = {
            type = "app";
            program = "${test}/bin/cl-http-message-kit-test";
          };
          test = {
            type = "app";
            program = "${test}/bin/cl-http-message-kit-test";
          };
        }
      );
    };
}
