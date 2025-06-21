{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in {
    packages.x86_64-linux.default = pkgs.buildEnv {
      name = "my-packages-list";
      paths = with pkgs; [
        fzf
        github-mcp-server
        ghq
        hackgen-font
        hackgen-nf-font
      ];
    };
  };
}
