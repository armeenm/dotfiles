{ lib, user, ... }:

{
  home-manager = {
    extraSpecialArgs = {
      enableSocial = true;
      isPortable = true;
    };

    users.${user.login}.home.stateVersion = lib.mkForce "25.11";
  };

  determinateNix.buildMachines = [
    {
      hostName = "server0";
      protocol = "ssh-ng";
      sshUser = "it";
      sshKey = "/etc/nix/id_ed25519";
      publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUdTQTJUdlI5amhNQXZ0czd6YzBDeHByVlhoeWYvcWU1Qkprb2I4Q1JyZ2Egcm9vdEBzZXJ2ZXIwCg==";
      maxJobs = 64;
      supportedFeatures = [
        "big-parallel"
        "kvm"
      ];
      systems = [
        "aarch64-linux"
        "x86_64-linux"
      ];
    }
  ];

  system.stateVersion = 6;
}
