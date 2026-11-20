{ lib, pkgs, user, ... }:

{
  home-manager = {
    extraSpecialArgs.enableSocial = true;
    users.${user.login}.home = {
      stateVersion = lib.mkForce "25.11";

      # Moonlight client: the TV runs macOS, so this is the end that receives
      # the stream from Sunshine on argentum. SwitchAudioSource moves the
      # default output to the HDMI sink, which macOS will not do from ssh.
      #
      # sunshine is the reverse direction: it lets a Moonlight client on
      # argentum (or anywhere) drive this Mac's desktop, which beats VNC for
      # the occasional remote fix.
      packages = [ pkgs.moonlight-qt pkgs.switchaudio-osx pkgs.sunshine ];

      file.".config/sunshine/sunshine.conf".text = ''
        sunshine_name = rhenium
        lan_encryption_mode = 1
        origin_web_ui_allowed = pc
      '';
    };
  };

  # Sunshine is not a NixOS-only service here: nix-darwin has no module for it,
  # so it runs as a user agent in the logged-in session, which is what gives it
  # access to the WindowServer for capture and input.
  launchd.user.agents.sunshine = {
    serviceConfig = {
      ProgramArguments = [
        "${pkgs.sunshine}/bin/sunshine"
        "/Users/${user.login}/.config/sunshine/sunshine.conf"
      ];
      RunAtLoad = true;
      KeepAlive.SuccessfulExit = false;
      ProcessType = "Interactive";
      StandardOutPath = "/Users/${user.login}/Library/Logs/sunshine.log";
      StandardErrorPath = "/Users/${user.login}/Library/Logs/sunshine.log";
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

  system.stateVersion = 6;
}
