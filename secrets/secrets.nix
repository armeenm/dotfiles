let
  armeen-yk0 = "age1yubikey1qvdrue6hh6hnrvn0md3txjg0p6rv504ujhkkta0tw8dzuh69rlwtgwjfng2";

  lithium = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJl6/aiWgReQ80Tbde4fLE1vdM49alVEREXnUIN5NDVP";
  carbon = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIy8r+XjFmIpZFDTnJxJ8mhqXnayILGziupst+1RFYtA";
  hosts = [ lithium carbon ];

in {
  "armeen-pw.age".publicKeys = [ armeen-yk0 ] ++ hosts;
  "arash-pw.age".publicKeys = [ armeen-yk0 lithium ];
  "restic-pw.age".publicKeys = [ armeen-yk0 carbon ];
  "restic-b2-env.age".publicKeys = [ armeen-yk0 carbon ];
}
