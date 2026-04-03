let
  armeen-yk0 = "age1yubikey1qvdrue6hh6hnrvn0md3txjg0p6rv504ujhkkta0tw8dzuh69rlwtgwjfng2";
  armeen-yk1 = "age1yubikey1qtkxz9jqzsyjwa57wm7hhdqwpqsuufctegulcckfa3pu4h5rsrf47yr7e38";

  argentum = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICZQdVbbPwRm4+e3njANkGEAwMTp8kiBy6gzb5QwPslU";
  lithium = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJl6/aiWgReQ80Tbde4fLE1vdM49alVEREXnUIN5NDVP";
  carbon = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIy8r+XjFmIpZFDTnJxJ8mhqXnayILGziupst+1RFYtA";
  hosts = [ argentum lithium carbon ];

in {
  "armeen-pw.age".publicKeys = [ armeen-yk0 ] ++ hosts;
  "arash-pw.age".publicKeys = [ armeen-yk0 ] ++ hosts;
  "restic-pw.age".publicKeys = [ armeen-yk0 carbon ];
  "restic-b2-env.age".publicKeys = [ armeen-yk0 carbon ];
  "vaultwarden-env.age".publicKeys = [ armeen-yk0 carbon ];
  "cobalt.yaml.age".publicKeys = [ armeen-yk0 carbon ];
  "cloudflare-api-token.age".publicKeys = [ armeen-yk0 carbon ];
}
