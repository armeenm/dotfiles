{ lib, pkgs, user, ... }:

# A deliberately small NixOS profile for appliances that only need to be
# reached over SSH. It has no home-manager, no firmware blobs and no graphical
# stack, so the system closure fits comfortably on a 16 GiB SD card.
{
  imports = [
    ./base.nix
    ../shared/cache.nix
  ];

  time.timeZone = "UTC";

  # Manuals are the largest avoidable chunk of a default NixOS closure.
  documentation = {
    enable = false;
    doc.enable = false;
    info.enable = false;
    man.enable = false;
    nixos.enable = false;
  };

  environment.systemPackages = with pkgs; [
    htop
    rsync
    vim
  ];

  # The default list pulls in LVM, XFS, Btrfs and bcachefs tools. This
  # appliance only ever sees ext4 and the FAT firmware partition.
  boot.supportedFilesystems = lib.mkForce [ "ext4" "vfat" ];

  # No LVM or bcache on a plain SD-card install.
  services.lvm.enable = false;
  boot.bcache.enable = false;

  # Deploys happen through deploy-rs, which only needs
  # switch-to-configuration. Dropping the installer tools removes
  # nixos-rebuild and nixos-generate-config plus their dependency closures.
  system.disableInstallerTools = true;

  # Do not pin the full nixpkgs source in /etc/nix/registry.json; on this card
  # it costs about 200 MiB for no benefit.
  nixpkgs.flake = {
    setFlakeRegistry = false;
    setNixPath = false;
  };

  nix.settings = {
    auto-optimise-store = true;
    experimental-features = [ "nix-command" "flakes" ];

    # The big hosts reserve 10 GiB for a rebuild; that would prevent every
    # rebuild on a 16 GiB card.
    min-free = 1024 * 1024 * 1024;
  };

  networking = {
    useDHCP = true;

    # netfilter lives exclusively in nftables on this host: this makes the
    # NixOS firewall use its nftables backend and blacklists ip_tables.
    nftables.enable = true;

    firewall.enable = true;
  };

  # No password is set for the user, so passwordless sudo is what makes
  # administrative access over an SSH key work.
  security.sudo-rs.extraRules = [
    {
      users = [ user.login ];
      commands = [
        {
          command = "ALL";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];

  # Public key only; it is the same `armeen-ssh` key listed in
  # secrets/secrets.nix.
  users.users."${user.login}".openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDXWQumRO6nC4ABA79WHB3kkNj0vUqAiz8oJwbxeG457 armeen"
  ];
}
