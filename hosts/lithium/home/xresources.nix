{ config, pkgs, lib, root, user, ... }:

{
  path = "${config.xdg.configHome}/xresources";
  
  extraConfig = builtins.readFile (
    pkgs.fetchFromGitHub {
      owner = "dracula";
      repo = "xresources";
      rev = "8de11976678054f19a9e0ec49a48ea8f9e881a05";
      sha256 = "p8/E7nA+A5niKsqkO7/c3iDkINyTPAgWf91nMK2XlYs=";
    } + "/Xresources"
  );
}
