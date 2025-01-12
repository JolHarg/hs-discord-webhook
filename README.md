# discord-webhook

Discord webhook for use with OpenFAAS.

Only for certain event types, none of which are of use to me right now, it turns out.

https://discord.com/developers/docs/events/webhook-events#event-types

## Usage

Setup:
```bash
doctl sls install
doctl sls ns create -l bots -r lon1
doctl sls connect bots
```

Deployment:

```bash
doctl sls deploy .
doctl sls fn get bots/handler --url
```

Undeployment:

```bash
doctl sls undeploy .
```

Unsetup:

```bash
doctl sls ns rm -f bots
doctl sls uninstall
```