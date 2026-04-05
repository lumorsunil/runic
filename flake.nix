{
  description = "Runic - A modern scripting language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    packages.${system} = {
      default = pkgs.stdenv.mkDerivation {
        pname = "runic";
        version = "0.1.0";

        src = self;

        nativeBuildInputs = [ pkgs.zig ];

        buildPhase = ''
          export HOME=$TMPDIR
          zig build
        '';

        installPhase = ''
          mkdir -p $out/bin
          cp zig-out/bin/runic $out/bin/
          cp zig-out/bin/runic-lsp $out/bin/
        '';

        meta = with pkgs.lib; {
          description = "A modern scripting language inspired by bash";
          homepage = "https://github.com/ijadux2/runic";
          license = licenses.mit;
          platforms = platforms.linux;
        };
      };
    };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.default;
  };
}
