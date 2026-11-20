{ lib, pkgs, user, ... }:

{
  home-manager = {
    extraSpecialArgs.enableSocial = true;
    users.${user.login}.home = {
      stateVersion = lib.mkForce "25.11";

      # Moonlight client: the TV runs macOS, so this is the end that receives
      # the stream from Sunshine on argentum. SwitchAudioSource moves the
      # default output to the HDMI sink, which macOS will not do from ssh.
      packages = [ pkgs.moonlight-qt pkgs.switchaudio-osx ];
    };
  };

  # rhenium is a desktop Mac mini that is administered over SSH, so it must
  # stay awake: with the macOS default idle timer the system enters
  # 'Maintenance Sleep' after one idle minute, drops off the LAN (no ICMP, no
  # TCP) and only a Wake-on-LAN packet brings it back. Keep display sleep.
  power.sleep = {
    computer = "never";
    display = 10;
    harddisk = "never";
  };

  # Stealth mode makes the Application Firewall drop ICMP echo requests, so
  # the Mac answers nothing on the LAN and every liveness check has to be TCP
  # (it could not even ping itself). Keep the firewall on, drop that behaviour.
  networking.applicationFirewall.enableStealthMode = false;

  # `deploy .#rhenium` copies this flake to the Mac and runs nix-darwin's
  # activation script as root, which is a different command than
  # darwin-rebuild: the rule in /etc/sudoers.d/armeen-updates covers
  # darwin-rebuild only, so deploys would still prompt. Allow just the
  # activation script out of the store.
  security.sudo.extraConfig = ''
    armeen ALL=(root) NOPASSWD: /nix/store/*-darwin-system-*/activate
  '';

  # AirPlay receiver: NOT pinnable here. On 26.7.3 the receiver's state lives
  # in no plist: neither /Library/Preferences/com.apple.controlcenter.plist
  # (the historical, misspelled `AirplayRecieverEnabled`) nor any
  # com.apple.airplay domain exists, and writing both candidates changed
  # nothing — the receiver kept advertising on `_airplay._tcp`. The toggle is
  # owned by AirDropHandoffExtension (`airPlayAllowedKey`,
  # `_allowAirPlayReceiver`) through a private API. Leave it at the macOS
  # default, which survived 26.5.1 -> 26.7.3 unaided; if an upgrade ever turns
  # it off, flip it once in System Settings > General > AirDrop & Handoff.

  system.stateVersion = 6;
}
