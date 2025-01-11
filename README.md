# bots

Bots for use with OpenFAAS.

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