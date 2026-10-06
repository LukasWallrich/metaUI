# Platform and browser checks

The package-check matrix runs R release on GitHub-hosted Linux, macOS and Windows,
plus oldrel-1 on Linux. All jobs execute the independent model/reference tests and
the generated Shiny server tests. These are compatibility checks on the selected
runner architectures, not a claim to test every OS/R/CPU combination.

A separate Linux Chromium job installs the package, builds an editable standalone
app in a fresh runner directory, then launches it in a separate R process. Its
synthetic 16-effect fixture tests initial analysis, cache disclosure, forest image
rendering, workbook download/re-upload, restored picker selections, missing-value filtering,
numeric filtering and reset. The workflow attempts to upload screenshots, Shiny logs and a
machine-readable result as `generated-app-browser-evidence` on failure too, but the
available evidence depends on how far the job gets. There is no
public app deployment. The workflow token has read-only repository permissions.

Reproduce after installing metaUI and its declared dependencies:

```sh
Rscript --vanilla tools/browser-fixture.R /tmp/metaui-smoke-fresh
# With Playwright and its Chromium installed in your chosen scratch environment:
METAUI_SMOKE_DIR=/tmp/metaui-smoke-fresh node tools/browser-smoke.cjs
```

The fixture destination must not exist. Build and launch are separate actions;
the Node script starts and stops only its own R child process. CI uses Playwright
1.63.0 in the runner scratch directory. To reuse an existing installation locally,
set `METAUI_PLAYWRIGHT` to its module path and `METAUI_CHROMIUM` to its browser
executable; no browser installation on the box is needed. `METAUI_SMOKE_PORT`
(default 8765) can select a free local port. Evidence stays in the fixture's
`evidence` directory and is not a production dataset.

Further validation should include a short Safari/Firefox check and an independent
author trying the creation workflow. It cannot establish the scientific appropriateness of arbitrary
inputs or actual research adoption. Scientific model equivalence is checked against
independent metafor/robumeta calculations in the package suite, rather than inferred
from rendered plots. A separate literal container is not required for this matrix.
