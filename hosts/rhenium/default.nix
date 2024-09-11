{ config, pkgs, lib, inputs, root, user, ... }:

let 
  hostPlatform = "aarch64-darwin";
in {
  nixpkgs = {
    inherit hostPlatform;
    config.allowUnfree = true;
  };

  nix = {
    package = pkgs.nixVersions.latest;
    channel.enable = true;
    nixPath = lib.mkForce [ "nixpkgs=${config.nix.registry.nixpkgs.flake}" ];

    registry = {
      nixpkgs.flake = inputs.nixpkgs;
    };

    settings = {
      allowed-users = lib.mkForce [ "@everyone" ];
      trusted-users = lib.mkForce [ "@admin" ];

      experimental-features = [
        "ca-derivations"
        "flakes"
        "nix-command"
        "recursive-nix"
      ];

      warn-dirty = false;
    };
  };

  users.users.${user.login} = {
    name = "armeen";
    home = "/Users/armeen";
  };

  home-manager = {
    users."${user.login}" = import "${root}/home";
    extraSpecialArgs = {
      inherit inputs root user;
      stateVersion = "24.11";
    };
  };

  environment = {
    systemPackages = [
    ] ++ (with pkgs; [
      git
    ]);
  };

  programs = {
    zsh.enable = true;
  };

  services = {
    nix-daemon.enable = true;
  };

  homebrew = {
    enable = true;
  };
}
