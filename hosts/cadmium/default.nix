{ lib, user, pkgs, ... }:

{
  home-manager = {
    extraSpecialArgs = {
      enableSocial = true;
      isPortable = true;
    };

    users.${user.login}.home.stateVersion = lib.mkForce "26.05";
  };

  determinateNix.buildMachines = let
    x = pkgs.runCommandLocal "foo" { __impure = true; } ''
      export PATH=$PATH:${pkgs.age-plugin-se}/bin
      whoami
      ${pkgs.rage}/bin/rage -d ${../../secrets/itmaclap-machines.nix.age} -i ${../../secrets/identities/armeen-itmaclap.txt} > $out
    '';

  in [
    {
      hostName = "server0.corp.edpi.ai";
      protocol = "ssh-ng";
      sshUser = "it";
      sshKey = "/etc/nix/id_ed25519";
      publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUdTQTJUdlI5amhNQXZ0czd6YzBDeHByVlhoeWYvcWU1Qkprb2I4Q1JyZ2Egcm9vdEBzZXJ2ZXIwCg==";
      maxJobs = 16;
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "recursive-nix"
      ];
      systems = [
        "aarch64-linux"
        "x86_64-linux"
      ];
    }
  ];

  system.stateVersion = 6;
}
