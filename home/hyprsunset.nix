{ config
, lib
, pkgs
, isHeadless
, ...
}@args:

let
  inherit (pkgs.stdenv) hostPlatform;

  enabled = !isHeadless && hostPlatform.isLinux;

  pinnedLocation = args.solarLocation or null;
  fallbackLocation = {
    latitude = 47.6062;
    longitude = -122.3321;
  };

  pinnedLocationValue =
    if pinnedLocation == null then ""
    else "${toString pinnedLocation.latitude},${toString pinnedLocation.longitude}";

  fallbackLocationValue =
    "${toString fallbackLocation.latitude},${toString fallbackLocation.longitude}";

  # The client must match the compositor it talks to, and `pkgs.hyprland` is
  # whatever the flake's nixpkgs pinned rather than what is running.
  hyprlandPackage = config.wayland.windowManager.hyprland.package;

  dayElevation = 6.0;
  nightElevation = -6.0;
  dayTemperature = 6500;
  nightTemperature = 3500;

  solar = pkgs.writers.writePython3Bin "hyprsunset-solar" { } ''
    """Set the hyprsunset temperature from the sun's elevation."""

    import math
    import os
    import subprocess
    import sys
    import time
    import urllib.request
    from datetime import datetime, timezone

    IP = "${pkgs.iproute2}/bin/ip"  # noqa: E501
    HYPRCTL = "${hyprlandPackage}/bin/hyprctl"  # noqa: E501

    PINNED = "${pinnedLocationValue}"
    FALLBACK = "${fallbackLocationValue}"

    DAY_ELEVATION = ${toString dayElevation}
    NIGHT_ELEVATION = ${toString nightElevation}
    DAY_TEMPERATURE = ${toString dayTemperature}
    NIGHT_TEMPERATURE = ${toString nightTemperature}

    # A degree of latitude moves sunset by about four minutes, so city level
    # is plenty. One keyless endpoint eventually answers 429, hence two.
    PROVIDERS = (
        "https://ipinfo.io/loc",
        "https://ipapi.co/latlong/",
    )

    GEOIP_MAX_AGE = 6 * 3600
    FAILURE_MAX_AGE = 15 * 60
    APPLY_ATTEMPTS = 5
    APPLY_DELAY = 0.5
    HTTP_TIMEOUT = 5
    PROBE_ADDRESS = "9.9.9.9"

    IDENTITY_THRESHOLD = 6400


    def parse_pair(text):
        """Return (lat, lon) from "lat,lon", or None if it is not that."""
        fields = text.strip().split(",")
        if len(fields) != 2:
            return None
        try:
            latitude, longitude = (float(field) for field in fields)
        except ValueError:
            return None
        if not -90 <= latitude <= 90:
            return None
        if not -180 <= longitude <= 180:
            return None
        return latitude, longitude


    PINNED_LOCATION = parse_pair(PINNED) if PINNED else None
    FALLBACK_LOCATION = parse_pair(FALLBACK)


    def solar_elevation(when, latitude, longitude):
        """Solar elevation in degrees (NOAA approximation)."""
        day = when.timetuple().tm_yday
        hour = when.hour + when.minute / 60 + when.second / 3600

        gamma = 2 * math.pi / 365 * (day - 1 + (hour - 12) / 24)
        equation_of_time = 229.18 * (
            0.000075
            + 0.001868 * math.cos(gamma)
            - 0.032077 * math.sin(gamma)
            - 0.014615 * math.cos(2 * gamma)
            - 0.040849 * math.sin(2 * gamma)
        )
        declination = (
            0.006918
            - 0.399912 * math.cos(gamma)
            + 0.070257 * math.sin(gamma)
            - 0.006758 * math.cos(2 * gamma)
            + 0.000907 * math.sin(2 * gamma)
            - 0.002697 * math.cos(3 * gamma)
            + 0.00148 * math.sin(3 * gamma)
        )

        solar_time = hour * 60 + equation_of_time + 4 * longitude
        hour_angle = math.radians(solar_time / 4 - 180)
        latitude_radians = math.radians(latitude)

        sine = (
            math.sin(latitude_radians) * math.sin(declination)
            + math.cos(latitude_radians)
            * math.cos(declination)
            * math.cos(hour_angle)
        )
        return math.degrees(math.asin(max(-1.0, min(1.0, sine))))


    def temperature_for(elevation):
        """Map elevation to Kelvin, 6500 by day and 3500 once dark."""
        if elevation >= DAY_ELEVATION:
            return DAY_TEMPERATURE
        if elevation <= NIGHT_ELEVATION:
            return NIGHT_TEMPERATURE

        span = DAY_ELEVATION - NIGHT_ELEVATION
        ramp = (DAY_ELEVATION - elevation) / span
        ramp = ramp * ramp * (3 - 2 * ramp)
        kelvin = DAY_TEMPERATURE - ramp * (DAY_TEMPERATURE - NIGHT_TEMPERATURE)

        # 50 K steps are below perception but keep the journal quiet.
        return int((kelvin + 25) / 50) * 50


    def run(command):
        """Run a command, returning stdout or None when it cannot run."""
        try:
            result = subprocess.run(
                command,
                capture_output=True,
                text=True,
                timeout=HTTP_TIMEOUT,
                check=False,
            )
        except (OSError, subprocess.SubprocessError):
            return None
        if result.returncode != 0:
            return None
        return result.stdout


    def egress_is_tunnel():
        """Would a request to the internet leave through a tunnel?

        Names are no help: Cloudflare WARP calls its interface
        CloudflareWARP and Tailscale calls its one tailscale0. Ask the kernel
        instead -- TUN/TAP devices expose tun_flags, and WireGuard, PPP and
        IPsec report ARPHRD_NONE (65534) where a real link reports
        ARPHRD_ETHER (1).
        """
        route = run([IP, "route", "get", PROBE_ADDRESS])
        if route is None:
            return False

        fields = route.split()
        if "dev" not in fields:
            return False
        device = fields[fields.index("dev") + 1]
        if not device:
            return False

        sysfs = os.path.join("/sys/class/net", device)
        if os.path.exists(os.path.join(sysfs, "tun_flags")):
            return True
        if device == "lo":
            return False

        try:
            with open(os.path.join(sysfs, "type"), encoding="utf-8") as handle:
                kind = handle.read().strip()
        except OSError:
            return False
        return kind != "1"


    def fetch_location():
        """Ask a GeoIP service where this machine is, or None."""
        for url in PROVIDERS:
            try:
                with urllib.request.urlopen(url, timeout=HTTP_TIMEOUT) as f:
                    body = f.read(200).decode("utf-8", "replace")
            except (OSError, ValueError):
                continue
            location = parse_pair(body)
            if location is not None:
                return location
        return None


    def cache_file():
        base = os.environ.get("XDG_CACHE_HOME")
        if not base:
            base = os.path.expanduser("~/.cache")
        return os.path.join(base, "hyprsunset", "location")


    def read_cache(path):
        try:
            with open(path, encoding="utf-8") as handle:
                fields = handle.read().split()
        except OSError:
            return None
        if len(fields) != 4:
            return None
        try:
            return (
                float(fields[0]),
                float(fields[1]),
                int(fields[2]),
                fields[3],
            )
        except ValueError:
            return None


    def write_cache(path, latitude, longitude, source):
        os.makedirs(os.path.dirname(path), exist_ok=True)
        stamp = int(time.time())
        with open(path, "w", encoding="utf-8") as handle:
            handle.write(
                f"{latitude:.4f} {longitude:.4f} {stamp} {source}\n"
            )


    def resolve_location():
        """Coordinates to use, from the pin, the cache, or the network."""
        if PINNED_LOCATION is not None:
            return PINNED_LOCATION

        path = cache_file()
        cached = read_cache(path)
        tunneled = egress_is_tunnel()

        # Behind a tunnel the last direct observation stays authoritative
        # however old it is: ageing it out would replace a good location with
        # the exit node's.
        if cached is not None:
            max_age = (
                GEOIP_MAX_AGE if cached[3] == "geoip" else FAILURE_MAX_AGE
            )
            if tunneled or time.time() - cached[2] < max_age:
                return cached[0], cached[1]

        if tunneled:
            print(
                "traffic leaves through a tunnel and nothing is cached, "
                "using the compiled fallback",
                file=sys.stderr,
            )
            return FALLBACK_LOCATION

        fetched = fetch_location()
        if fetched is not None:
            write_cache(path, fetched[0], fetched[1], "geoip")
            return fetched

        # A transient failure is no reason to forget where the machine last
        # was, so keep the stale coordinates and retry soon.
        if cached is not None:
            latitude, longitude = cached[0], cached[1]
        else:
            latitude, longitude = FALLBACK_LOCATION
        print(
            f"location lookup failed, keeping {latitude},{longitude}",
            file=sys.stderr,
        )
        write_cache(path, latitude, longitude, "fallback")
        return latitude, longitude


    def apply_temperature(temperature):
        # hyprsunset's identity matrix is exact; its 6500 K matrix only rounds
        # to one, so prefer the former at the unfiltered end of the curve.
        if temperature >= IDENTITY_THRESHOLD:
            command = [HYPRCTL, "hyprsunset", "identity"]
        else:
            command = [HYPRCTL, "hyprsunset", "temperature", str(temperature)]
        return run(command) is not None


    def main():
        # Nothing to talk to before Hyprland starts or after it exits; a tick
        # that lands outside a session is not an error.
        if run([HYPRCTL, "version"]) is None:
            return 0

        latitude, longitude = resolve_location()
        now = datetime.now(timezone.utc)
        temperature = temperature_for(
            solar_elevation(now, latitude, longitude)
        )

        # hyprsunset opens its own socket just after it starts, while
        # `hyprctl version` only proves Hyprland is up.
        for attempt in range(1, APPLY_ATTEMPTS + 1):
            if apply_temperature(temperature):
                break
            if attempt == APPLY_ATTEMPTS:
                print(
                    f"hyprsunset did not accept a temperature after "
                    f"{attempt} tries",
                    file=sys.stderr,
                )
                return 1
            time.sleep(APPLY_DELAY)

        # One line per tick, so `systemctl --user status hyprsunset-solar`
        # shows recent history and the journal keeps all of it.
        print(f"{temperature}K at {latitude:.4f},{longitude:.4f}")
        return 0


    if __name__ == "__main__":
        sys.exit(main())
  '';
in {
  config = lib.mkIf enabled {
    services.hyprsunset = {
      enable = true;

      # No `profile` here: the timer below is the scheduler. Start neutral so
      # the first tick is the only thing that ever sets a temperature.
      extraArgs = [ "--identity" ];
    };

    # hyprsunset comes up neutral, so re-apply the computed temperature as
    # soon as it starts — including after its own Restart=always, which would
    # otherwise leave the screen unfiltered until the next timer tick.
    systemd.user.services.hyprsunset.Service.ExecStartPost =
      [ "${pkgs.systemd}/bin/systemctl --user --no-block start hyprsunset-solar.service" ];

    systemd.user = {
      services.hyprsunset-solar = {
        Install.WantedBy = [ config.wayland.systemd.target ];

        Unit = {
          ConditionEnvironment = "WAYLAND_DISPLAY";
          Description = "Set the hyprsunset temperature from the sun's elevation";
          After = [ config.wayland.systemd.target "hyprsunset.service" ];
          PartOf = [ config.wayland.systemd.target ];
        };

        Service = {
          Type = "oneshot";
          ExecStart = lib.getExe solar;

          # The single line the script prints is the temperature history;
          # keep stderr on the journal so a failure is never folded into it.
          StandardError = "journal";
        };
      };

      timers.hyprsunset-solar = {
        Install.WantedBy = [ config.wayland.systemd.target ];

        Unit = {
          Description = "Recompute the hyprsunset temperature";
          PartOf = [ config.wayland.systemd.target ];
        };

        Timer = {
          # The first tick lands as soon as the session target is up.
          OnBootSec = "15s";
          OnUnitActiveSec = "5min";
          AccuracySec = "15s";
          Unit = "hyprsunset-solar.service";
        };
      };
    };
  };
}
