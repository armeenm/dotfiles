# Cloudflare (Pulumi)

```sh
nix develop
export CLOUDFLARE_API_TOKEN="$(rage -d -i secrets/identities/armeen-yk1.txt secrets/pulumi-cloudflare-token.age)"
cd infra/cloudflare
pulumi stack select prod
pulumi preview
pulumi up
```

On the Mac, use `secrets/identities/armeen-itmaclap.txt` instead.
