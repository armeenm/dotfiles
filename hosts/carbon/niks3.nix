{ config, lib, pkgs, user, ... }:

let
  # Cloudflare account that owns the R2 bucket.
  accountId = "42dfd2dd6109b5d2d1099121f718f29d";
  bucket = "armeen-xyz-cache";
in
{
  age.secrets = {
    # Read by the niks3 service (group) and by armeen for `niks3 push`.
    niks3-api-token = {
      file = ../../secrets/niks3-api-token.age;
      owner = user.login;
      group = "niks3";
      mode = "0440";
    };

    niks3-signing-key = {
      file = ../../secrets/niks3-signing-key.age;
      owner = "niks3";
      group = "niks3";
      mode = "0400";
    };

    niks3-r2-access-key = {
      file = ../../secrets/niks3-r2-access-key.age;
      owner = "niks3";
      group = "niks3";
      mode = "0400";
    };

    niks3-r2-secret-key = {
      file = ../../secrets/niks3-r2-secret-key.age;
      owner = "niks3";
      group = "niks3";
      mode = "0400";
    };
  };

  services.niks3 = {
    enable = true;
    httpAddr = "[::1]:8010";

    # Reads go straight from R2; niks3 only serves the push API, GC, stats and
    # the landing page that it uploads into the bucket.
    cacheUrl = "https://cache.armeen.xyz";
    serverUrl = "https://niks3.armeen.xyz";
    priority = 30;

    apiTokenFile = config.age.secrets.niks3-api-token.path;
    signKeyFiles = [ config.age.secrets.niks3-signing-key.path ];

    s3 = {
      endpoint = "${accountId}.r2.cloudflarestorage.com";
      inherit bucket;
      useSSL = true;
      accessKeyFile = config.age.secrets.niks3-r2-access-key.path;
      secretKeyFile = config.age.secrets.niks3-r2-secret-key.path;
    };

    nginx = {
      enable = true;
      domain = "niks3.armeen.xyz";
      # Push requests list every object in the closure.
      clientMaxBodySize = "1g";
    };

    # 30 days, matching the default; niks3's GC is the only thing that deletes
    # objects from the bucket.
    gc.olderThan = "720h";
  };

  # Metrics are unauthenticated and this vhost is public.
  services.nginx.virtualHosts."niks3.armeen.xyz".locations."= /metrics".return = "404";

  # The push CLI, so carbon can seed the cache itself.
  environment.systemPackages = [ config.services.niks3.package ];

  # The server reads its S3 credentials and signing key from /run/agenix and
  # resolves the R2 endpoint through unbound. Order it after both so it does
  # not fail once on every boot/switch.
  systemd.services.niks3 = {
    after = [ "agenix-install-secrets.service" "unbound.service" ];
    wants = [ "unbound.service" ];
    requires = [ "agenix-install-secrets.service" ];
  };

  # R2 has no index document: serve the landing page explicitly from the API
  # hostname, whose root niks3 otherwise redirects to the bare cache root.
  services.nginx.virtualHosts."niks3.armeen.xyz".locations."= /".return =
    "302 https://cache.armeen.xyz/index.html";
}
