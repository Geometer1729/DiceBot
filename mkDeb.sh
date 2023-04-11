#!/bin/sh
nix bundle --bundler github:NixOS/bundlers#toDEB .#packages.x86_64-linux.default
