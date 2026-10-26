# Cloudflare (Pulumi)

Personal Cloudflare DNS. Everything is TypeScript; state lives in Pulumi Cloud
under the `hierarch/cloudflare-personal/prod` stack (the project name is
deliberately distinct from the `hierarch/cloudflare` project in the `hierarch`
repo, which manages a different Cloudflare account).

## What this stack owns

| Zone | Records |
| --- | --- |
| `armeen.xyz` | `ru` A; `carbon`/`cobalt`/`media`/`music`/`vault`/`www` CNAMEs; Google site verification TXT |
| `armeen.org` | `grpc` CNAME; `exam` AAAA (Worker route placeholder); Google + Keybase verification TXT |
| `fulminant-hill.com` / `.xyz` | Google site verification TXT |
| `fulmino.us` | `teleport.dash` + wildcard A; Google site verification TXT |
| `fulminous-hill.com` / `.xyz` | Google site verification TXT; `git` CNAME |
| `oreo.ooo` | `plague` A; `foto`/`ooo`/`status` CNAMEs; `_minecraft._tcp` SRV |
| `vermillionink.com` | apex + wildcard CNAMEs (parked at Porkbun) |
| `1984.team` | nothing — email records only |

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
- **`carbon` tunnel, `exam`/`hierarch` Workers, `fpgapres` Pages project** —
  deployed by Nix, wrangler, and the GitHub integration.

## Usage

The API token lives in `secrets/pulumi-cloudflare-token.age` (recipients:
your YubiKeys, the Mac, and carbon). It is a dedicated token named
`pulumi-cloudflare-personal` with `Zone:Read` + `DNS:Edit` on the ten zones
and a one-year TTL. The dyndns token is separate and stays `armeen.xyz`-only.

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
