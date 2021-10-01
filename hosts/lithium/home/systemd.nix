{ config, pkgs, lib, root, user, misc, ... }:

{
  user = {
    services = {
      ipfs = {
        Unit = {
          Description = "InterPlanetary File System (IPFS) daemon";
          Documentation = "https://docs.ipfs.io/";
          After = "network.target";
        };

        Service = {
          MemorySwapMax = 0;
          TimeoutStartSec = "infinity";
          Type = "notify";
          ExecStart = "${pkgs.ipfs}/bin/ipfs daemon --init --migrate";
          Restart = "on-failure";
          KillSignal = "SIGINT";
        };

        Install = {
          WantedBy = [ "default.target" ];
        };
      };
    };
  };
}
