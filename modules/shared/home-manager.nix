{ config , inputs , user , ...  }:

{
  home-manager = {
    users."${user.login}" = import ../../home;
    useGlobalPkgs = true;
    useUserPackages = true;

    extraSpecialArgs = {
      inherit inputs user;
      # NOTE: Must be overriden for Darwin.
      stateVersion = config.system.stateVersion;
      isHeadless = true;
      isStandalone = false;
      isPortable = false;
      enableSocial = false;
      cursorColor = null;
      cursorSize = 32;
    };
  };
}
