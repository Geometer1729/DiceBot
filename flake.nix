{
  description = "dice-bot";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-flake.url = "github:srid/treefmt-flake";
    discord-haskell = {
      #url = "github:discord-haskell/discord-haskell";
      url = "github:Geometer1729/discord-haskell"; # fork with bug fix pr is already open
      flake = false;
    };
    recursion-schemes = {
      url = "github:recursion-schemes/recursion-schemes";
      flake = false;
    };
    base64-bytestring = {
      url = "github:haskell/base64-bytestring?rev=6fbb78226a3a00174325ff9bbd98248fb3eb1130";
      flake = false;
    };
    wuss = {
      url = "github:tfausak/wuss?rev=7cab441d443b3a829cd7e04842aff25388b8f4ef";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-parts, haskell-flake, treefmt-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
        treefmt-flake.flakeModule
      ];
      perSystem = { self', config, pkgs, ... }: {
        haskellProjects.default = {
          root = ./.;
          buildTools = hp: {
            inherit (pkgs)
              treefmt;
          } // config.treefmt.formatters;
          source-overrides = {
            inherit (self.inputs)
              recursion-schemes
              discord-haskell
              base64-bytestring
              wuss
              ;
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
