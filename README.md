# bots

Bots for use with OpenFAAS.

## Usage

Once:

```bash
doctl sls install
```

Setup:
```bash
doctl sls ns create -l bots -r lon
doctl sls connect bots
```

Deployment:

```bash
doctl sls deploy .
```