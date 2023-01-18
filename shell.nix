let
  sysPkg = import <nixpkgs> { };
  releasedPkgs = sysPkg.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "22.11";
    sha256 = "sha256-/HEZNyGbnQecrgJnfE8d0WC5c1xuPSD2LUpB6YXlg4c=";
  };
  pkgs = import releasedPkgs {};
  stdenv = pkgs.stdenv;
  extraInputs = sysPkg.lib.optionals stdenv.isDarwin (with sysPkg.darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices]);

  dynamodb = stdenv.mkDerivation rec {
    pname = "dynamodb-local";
    version = "2020-12-17";

    src = sysPkg.fetchurl {
      url = "https://s3.us-west-2.amazonaws.com/dynamodb-local/dynamodb_local_2022-07-26.tar.gz";
      sha256 = "sha256-cr1ndfuiyuT/wtjoe7quJvUh+ggE2tEZ9WydPS8HO1w=";
    };
    sourceRoot = ".";

    buildInputs = [ sysPkg.makeWrapper ];

    installPhase = ''
      mkdir -p $out/lib
      cp -r DynamoDBLocal_lib $out/lib/
      cp  DynamoDBLocal.jar $out/lib
      makeWrapper ${sysPkg.jre}/bin/java $out/bin/dynamodb-local \
        --add-flags "-Djava.library.path=$out/lib/DynamoDBLocal_lib -jar $out/lib/DynamoDBLocal.jar -sharedDb"
    '';
  };


in stdenv.mkDerivation {
  name = "env";
  buildInputs = [ pkgs.gnumake
                  pkgs.erlangR25
                  pkgs.rebar3
                  pkgs.elixir_1_14
                  pkgs.wget
                  pkgs.beam.packages.erlang.elixir

                  pkgs.rebar
                  (pkgs.google-cloud-sdk.withExtraComponents [pkgs.google-cloud-sdk.components.cloud-datastore-emulator pkgs.google-cloud-sdk.components.pubsub-emulator])

                # UI
                  pkgs.nodejs
                  pkgs.jre
                  pkgs.foreman
                  dynamodb

                ] ++ extraInputs;
  shellHook = ''
        source .env

        export MIX_REBAR=$PWD/rebar3

        mix local.hex --force
        # cd ui
        # npm install
        # cd ..

  '';

}
