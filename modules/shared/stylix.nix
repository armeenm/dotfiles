{ pkgs, ... }:

{
  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/ayu-mirage.yaml";

    fonts = {
      sizes = {
        desktop = 14;
        popups = 12;
      };

      serif = {
        package = pkgs.crimson;
        name = "Crimson Pro";
      };

      sansSerif = {
        package = pkgs.lato;
        name = "Lato";
      };

      monospace = {
        package = pkgs.tamsyn;
        name = "Tamsyn";
      };
    };

    opacity = {
      applications = 0.8;
      desktop = 0.8;
      popups = 0.85;
      terminal = 0.8;
    };
  };
}
