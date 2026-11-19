{ lib, user, ... }:

{
  home-manager = {
    extraSpecialArgs.enableSocial = true;
    users.${user.login}.home.stateVersion = lib.mkForce "25.11";
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
