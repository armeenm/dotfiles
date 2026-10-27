import * as pulumi from "@pulumi/pulumi";
import * as cloudflare from "@pulumi/cloudflare";

const config = new pulumi.Config();

/**
 * Personal Cloudflare DNS.
 *
 * Deliberately NOT managed here:
 *  - The apex A record for armeen.xyz. services.cloudflare-dyndns on carbon
 *    owns it and rewrites it when the WAN address changes.
 *  - MX / SPF / DMARC / DKIM (`*._domainkey`). Owned by Cloudflare Email
 *    Routing or Google Workspace and refreshed by them.
 *  - _acme-challenge TXT. Transient ACME validation.
 *  - home.armeen.org. Points at a residential address that is updated out of
 *    band; pinning it here would cause flapping.
 *  - siggen.armeen.org. A Cloudflare Pages custom domain; Pages adds and
 *    removes the record itself.
 *  - The `carbon` tunnel, the `exam`/`hierarch` Workers, and the `fpgapres`
 *    Pages project. Deployed by Nix, wrangler, and the GitHub integration.
 *
 * Zone IDs are stable identifiers, not secrets, so they live here rather than
 * in stack config. Only the account ID stays configurable.
 */

export const accountId = config.require("accountId");

const zones = {
  armeenOrg: "b6cf5c6c466404348dbf8817cdabbd0d",
  armeenXyz: "f1a9a37b06d9b796a5d950a5db66b1cd",
  fulminantHillCom: "0500bd36089d69975e9676ee7c9d830d",
  fulminantHillXyz: "6e4c4ce51eb8fff5d612c09b47df9c51",
  fulminoUs: "6fac371abd785e85af2375baea77c48d",
  fulminousHillCom: "5d0b9177edbbfd7fdd38368b28542689",
  fulminousHillXyz: "c71e4c1d50c77db3b705164bec9107af",
  oreoOoo: "b49bc2caef26a3cebbf078043ee23637",
  vermillioninkCom: "ca69e872e1630b8aa6c0c7813d03ca84",
} as const;

const tunnelTarget = "904c1ace-669b-41c4-8b47-1641e63783f4.cfargotunnel.com";

type RecordSpec = {
  /** Pulumi resource name. Keep stable: it is the key in import.json. */
  resourceName: string;
  zoneId: string;
  name: string;
  type: string;
  content?: string;
  data?: cloudflare.types.input.DnsRecordData;
  proxied?: boolean;
  ttl?: number;
  comment?: string;
};

const records: RecordSpec[] = [
  // armeen.org
  { resourceName: "armeen-org-grpc", zoneId: zones.armeenOrg, name: "grpc.armeen.org", type: "CNAME", content: "ghs.googlehosted.com" },
  { resourceName: "armeen-org-google-site-verification", zoneId: zones.armeenOrg, name: "armeen.org", type: "TXT", content: "google-site-verification=CH7afV0wk5-bB8aNYWnilqh2UeL7L9z1kVVo2xMgEHM" },
  // Placeholder for the `exam` Worker route.
  { resourceName: "armeen-org-exam", zoneId: zones.armeenOrg, name: "exam.armeen.org", type: "AAAA", content: "100::", proxied: true },

  // armeen.xyz
  { resourceName: "armeen-xyz-ru", zoneId: zones.armeenXyz, name: "ru.armeen.xyz", type: "A", content: "94.103.83.236" },
  { resourceName: "armeen-xyz-carbon", zoneId: zones.armeenXyz, name: "carbon.armeen.xyz", type: "CNAME", content: tunnelTarget, proxied: true },
  { resourceName: "armeen-xyz-cobalt", zoneId: zones.armeenXyz, name: "cobalt.armeen.xyz", type: "CNAME", content: "carbon.armeen.xyz", proxied: true },
  { resourceName: "armeen-xyz-media", zoneId: zones.armeenXyz, name: "media.armeen.xyz", type: "CNAME", content: "carbon.armeen.xyz", proxied: true },
  { resourceName: "armeen-xyz-music", zoneId: zones.armeenXyz, name: "music.armeen.xyz", type: "CNAME", content: "carbon.armeen.xyz", proxied: true },
  { resourceName: "armeen-xyz-vault", zoneId: zones.armeenXyz, name: "vault.armeen.xyz", type: "CNAME", content: "carbon.armeen.xyz", proxied: true },
  { resourceName: "armeen-xyz-www", zoneId: zones.armeenXyz, name: "www.armeen.xyz", type: "CNAME", content: "armeen.xyz", proxied: true },
  { resourceName: "armeen-xyz-google-site-verification", zoneId: zones.armeenXyz, name: "armeen.xyz", type: "TXT", content: "\"google-site-verification=0K9wDtomZl_iHLI7Pn3fTyqTYvXS8OACgR2mqwDtYGk\"" },

  // fulmino.us
  { resourceName: "fulmino-us-teleport", zoneId: zones.fulminoUs, name: "teleport.dash.fulmino.us", type: "A", content: "52.205.250.169" },
  { resourceName: "fulmino-us-teleport-wildcard", zoneId: zones.fulminoUs, name: "*.teleport.dash.fulmino.us", type: "A", content: "52.205.250.169" },
  { resourceName: "fulmino-us-google-site-verification", zoneId: zones.fulminoUs, name: "fulmino.us", type: "TXT", content: "google-site-verification=WKP1YapVDi0zGPhAk86j3ey4T7MBxhnoWRp6BLnWqE0", ttl: 3600 },

  // fulminous-hill.com / .xyz
  { resourceName: "fulminous-hill-com-google-site-verification", zoneId: zones.fulminousHillCom, name: "fulminous-hill.com", type: "TXT", content: "google-site-verification=HnMxn_qcDb3wLQ5KR0MiuGELTmFl5otDHLGcCSQbbSw" },
  { resourceName: "fulminous-hill-xyz-git", zoneId: zones.fulminousHillXyz, name: "git.fulminous-hill.xyz", type: "CNAME", content: "armeen.xyz" },
  { resourceName: "fulminous-hill-xyz-google-site-verification", zoneId: zones.fulminousHillXyz, name: "fulminous-hill.xyz", type: "TXT", content: "google-site-verification=XSU2PvHeFYo-KTWg3oCbuGXxeEZwg-2TAfHVcJlTuP0", ttl: 3600 },

  // oreo.ooo
  { resourceName: "oreo-ooo-plague", zoneId: zones.oreoOoo, name: "plague.oreo.ooo", type: "A", content: "104.59.229.242" },
  { resourceName: "oreo-ooo-foto", zoneId: zones.oreoOoo, name: "foto.oreo.ooo", type: "CNAME", content: "plague.oreo.ooo", comment: "immich" },
  { resourceName: "oreo-ooo-ooo", zoneId: zones.oreoOoo, name: "ooo.oreo.ooo", type: "CNAME", content: "plague.oreo.ooo", comment: "jellyfin" },
  { resourceName: "oreo-ooo-status", zoneId: zones.oreoOoo, name: "status.oreo.ooo", type: "CNAME", content: "plague.oreo.ooo", comment: "uptime-kuma" },
  { resourceName: "oreo-ooo-minecraft", zoneId: zones.oreoOoo, name: "_minecraft._tcp.oreo.ooo", type: "SRV", data: { priority: 0, weight: 0, port: 40000, target: "plague.oreo.ooo" } },

  // vermillionink.com (parked at Porkbun)
  { resourceName: "vermillionink-com-apex", zoneId: zones.vermillioninkCom, name: "vermillionink.com", type: "CNAME", content: "uixie.porkbun.com", ttl: 600 },
  { resourceName: "vermillionink-com-wildcard", zoneId: zones.vermillioninkCom, name: "*.vermillionink.com", type: "CNAME", content: "uixie.porkbun.com", ttl: 600 },
];

for (const spec of records) {
  new cloudflare.DnsRecord(spec.resourceName, {
    zoneId: spec.zoneId,
    name: spec.name,
    type: spec.type,
    content: spec.content,
    data: spec.data,
    proxied: spec.proxied ?? false,
    ttl: spec.ttl ?? 1,
    comment: spec.comment,
  });
}
