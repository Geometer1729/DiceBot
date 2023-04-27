{
  description = "dice-bot";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-flake.url = "github:srid/treefmt-flake";
    discord-haskell = {
      url = "github:discord-haskell/discord-haskell";
      flake = false;
    };
  };

  outputs = inputs@{ flake-parts, haskell-flake, treefmt-flake, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        haskell-flake.flakeModule
        treefmt-flake.flakeModule
      ];
      perSystem = { self', config, pkgs, ... }: {
        haskellProjects.default = {
          overrides = self: super: with pkgs.haskell.lib; {
            discord-haskell = dontCheck (self.callCabal2nix "discord-haskell" inputs.discord-haskell {});
          };
        };
        treefmt.formatters = {
          inherit (pkgs)
            nixpkgs-fmt;
          inherit (pkgs.haskellPackages)
            cabal-fmt
            fourmolu;
        };
      };
    };
}
