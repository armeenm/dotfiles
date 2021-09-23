{ pkgs, lib, seafile-shared, ... }:

with lib;

seafile-shared.overrideAttrs (old: {
  pythonPath = old.pythonPath ++ [ pkgs.python3.pkgs.future ];
})
