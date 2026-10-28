# Cloudflare (Pulumi)

Personal Cloudflare resources: DNS, plus the R2 bucket behind the public Nix
binary cache. Everything is TypeScript; state lives in Pulumi Cloud under the
`hierarch/cloudflare-personal/prod` stack (the project name is deliberately
distinct from the `hierarch/cloudflare` project in the `hierarch` repo, which
manages a different Cloudflare account).

## What this stack owns

| Zone | Records |
| --- | --- |
| `armeen.xyz` | `ru` A; `carbon`/`cobalt`/`media`/`music`/`niks3`/`vault`/`www` CNAMEs; `cache` (R2 custom domain); Google site verification TXT |
| `armeen.org` | `grpc` CNAME; `exam` AAAA (Worker route placeholder); Google + Keybase verification TXT |
| `fulminant-hill.com` / `.xyz` | Google site verification TXT |
| `fulmino.us` | `teleport.dash` + wildcard A; Google site verification TXT |
| `fulminous-hill.com` / `.xyz` | Google site verification TXT; `git` CNAME |
| `oreo.ooo` | `plague` A; `foto`/`ooo`/`status` CNAMEs; `_minecraft._tcp` SRV |
| `vermillionink.com` | apex + wildcard CNAMEs (parked at Porkbun) |
| `1984.team` | nothing — email records only |

R2 resources (account-level):

| Resource | Name | Purpose |
| --- | --- | --- |
| `R2Bucket` | `armeen-xyz-cache` | NARs, narinfos, build logs, landing page |
| `R2CustomDomain` | `cache.armeen.xyz` | public read origin (Cloudflare CDN, zero egress) |
| `R2BucketLifecycle` | `armeen-xyz-cache-lifecycle` | abort multipart uploads older than 1 day |

Zone rulesets:

| Ruleset | Phase | Purpose |
| --- | --- | --- |
| `armeen-xyz-cache-rules` | `http_request_cache_settings` | cache `/nar/*` at the edge for a year and metadata for 5 minutes |
| `armeen-xyz-cache-redirect` | `http_request_dynamic_redirect` | send `cache.armeen.xyz/` to the landing page |

## What it deliberately does not own

See the comment block at the top of `index.ts` for the full rationale. In
short:

- **Apex `armeen.xyz` A** — `cloudflare-dyndns` on carbon rewrites it.
- **`home.armeen.org` A** — residential address updated out of band.
- **MX / SPF / DMARC / `*._domainkey` DKIM** — Cloudflare Email Routing or
  Google Workspace refresh these.
- **`_acme-challenge` TXT** — transient ACME validation.
- **`siggen.armeen.org`** — Cloudflare Pages custom domain; Pages manages the
  record.
- **`cache.armeen.xyz` CNAME** — created and owned by the `R2CustomDomain`
  resource. Do not add a `DnsRecord` with that name.
- **The niks3 R2 API token** — a separate, bucket-scoped S3 credential created
  by hand and stored in agenix. It deliberately stays out of Pulumi state.
- **`carbon` tunnel, `exam`/`hierarch` Workers, `fpgapres` Pages project** —
  deployed by Nix, wrangler, and the GitHub integration.

## Usage

The API token lives in `secrets/pulumi-cloudflare-token.age` (recipients:
your YubiKeys, the Mac, and carbon). It is a dedicated token named
`pulumi-cloudflare-personal` with `Zone:Read` + `DNS:Edit` on the ten zones
plus account-level `Workers R2 Storage Read`/`Write` (bucket creation) and a
one-year TTL. The ruleset resources additionally need `Zone → Cache Rules →
Edit`, `Zone → Single Redirect → Edit`, `Account Rulesets → Edit` and
`Account Filter Lists → Edit`. The dyndns token is separate and stays
`armeen.xyz`-only.

```sh
nix develop
export CLOUDFLARE_API_TOKEN="$(rage -d -i secrets/identities/armeen-yk1.txt secrets/pulumi-cloudflare-token.age)"
cd infra/cloudflare
pulumi stack select prod
pulumi preview
pulumi up
```

On the Mac, use `secrets/identities/armeen-itmaclap.txt` instead. Both
identities require the usual YubiKey/Secure Enclave touch.

## Nix binary cache (niks3)

`cache.armeen.xyz` is a public Nix substituter backed by the
`armeen-xyz-cache` R2 bucket. Nix clients read from R2 through the custom
domain, with NARs cached at Cloudflare's edge for a year; the niks3 server on
carbon only handles pushes (via `niks3.armeen.xyz`), garbage collection, and
the landing page.

After the first `pulumi up`, create the S3 credential niks3 uses:

1. Cloudflare dashboard → R2 → Manage R2 API Tokens → Create API Token.
2. Permissions: **Object Read & Write**, scoped to `armeen-xyz-cache` only.
3. Store the access key ID and secret in agenix:

   ```sh
   ragenix -e secrets/niks3-r2-access-key.age
   ragenix -e secrets/niks3-r2-secret-key.age
   ```

The S3 endpoint is `<accountId>.r2.cloudflarestorage.com`; the bucket name is
`armeen-xyz-cache`.

So that `pulumi up` can create the bucket and custom domain, the Pulumi token
needs account-level `Workers R2 Storage Read` and `Workers R2 Storage Write`
in addition to the zone permissions above.

## Importing another record

```sh
pulumi import 'cloudflare:index/dnsRecord:DnsRecord' armeen-xyz-something '<zone_id>/<record_id>'
```

Add the generated declaration to `index.ts`, keep the resource name stable,
and re-run `pulumi preview` until it is clean. `import.json` is the bulk form
used for the initial import.

## Changing a record

Edit `records` in `index.ts` and `pulumi up`. Do not edit records in the
Cloudflare dashboard; the next `up` will revert them.
