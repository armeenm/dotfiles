# Cloudflare (Pulumi)

```sh
nix develop
export CLOUDFLARE_API_TOKEN="$(rage -d -i secrets/identities/armeen-yk1.txt secrets/pulumi-cloudflare-token.age)"
cd infra/cloudflare
pulumi stack select prod

# One-time: the bot-management setting already exists on the zone.
pulumi import 'cloudflare:index/botManagement:BotManagement' \
  armeen-xyz-bot-management f1a9a37b06d9b796a5d950a5db66b1cd

pulumi preview
pulumi up
```

On the Mac, use `secrets/identities/armeen-itmaclap.txt` instead.
