let
  armeen-yk0 = "age1yubikey1qvdrue6hh6hnrvn0md3txjg0p6rv504ujhkkta0tw8dzuh69rlwtgwjfng2";
  armeen-yk1 = "age1yubikey1qtkxz9jqzsyjwa57wm7hhdqwpqsuufctegulcckfa3pu4h5rsrf47yr7e38";
  armeen-itmaclap = "age1se1qdr6mst2npe3378tqnp32k8q7hsn6lrxn9g4m06p0yy366sundy0crs9vt5";
  armeen = [ armeen-yk0 armeen-yk1 armeen-itmaclap ];
  users = armeen;

  argentum = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICZQdVbbPwRm4+e3njANkGEAwMTp8kiBy6gzb5QwPslU";
  lithium = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJl6/aiWgReQ80Tbde4fLE1vdM49alVEREXnUIN5NDVP";
  carbon = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA9yCr5oGZrYNhHW2Od8/3sw8OCHbGtsDrBM6pp+y3Um";
  itmaclap = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDWccq1GfFOiFXaKir3KZ6hTiOZEsYDe41/ucTegc50b";
  hosts = [ argentum lithium carbon itmaclap ];

in {
  "armeen-pw.age".publicKeys = armeen ++ hosts;
  "arash-pw.age".publicKeys = armeen ++ hosts;
  "restic-pw.age".publicKeys = armeen ++ [ carbon ];
  "restic-b2-env.age".publicKeys = armeen ++ [ carbon ];
  "vaultwarden-env.age".publicKeys = armeen ++ [ carbon ];
  "cobalt.yaml.age".publicKeys = armeen ++ [ carbon ];
  "cloudflare-api-token.age".publicKeys = armeen ++ [ carbon ];
  "itmaclap-machines.nix.age".publicKeys = armeen ++ [ itmaclap ];
}
