# Author design helper

`design_app()` runs a local Shiny editor. It previews illustrative results using
`metaui.css` and the same generated overrides that app creation writes to
`www/app-design.css`. There are controls for four colours, four local font stacks,
base text size, compact/comfortable spacing and panel corner radius. The preview
can use a desktop or phone viewport. It does not fit or upload research data.

`app_design()` returns a plain named list. Pass it as `design` to
`generate_shiny()`, or embed the exported JSON object in `build_app()`'s `design`
field. Both JSON schema versions accept these presentation settings. Named-list
validation rejects unknown fields, duplicate names, malformed colours, unsupported
fonts and out-of-range sizes before creating output files. Colours are opaque hex;
no CSS or external font URLs can enter through these controls.

Text and accents need 4.5:1 contrast against page, panel and derived tinted
surfaces. Invalid editor choices retain the last valid preview and withhold the
code/downloads until corrected. The helper checks these combinations rather than
claiming a full accessibility audit of arbitrary author HTML or Bootstrap themes.

Generated apps retain a `design.json` record and editable CSS; they remain
standalone. With no explicit design, existing styling is retained. Rebuilding
with `overwrite = TRUE` and no design removes an earlier generated override file.
The primary-model comparison and forest plot use the selected accent, text and
panel colours. Other analytical plots retain their plotting packages' defaults;
the interface font selection does not change plot fonts or statistical results.

## Verification

- Full R suite passed with one optional `bayesmeta` test skipped.
- Focused tests cover R/JSON settings round trips, config builds, validation before
  writes, preserving input data, theme removal on rebuild, invalid-export recovery
  and label/background colours on dark forest plots.
- `tools/design-smoke.cjs` checks palette/font/size/spacing/corner controls, preview
  versus generated-app computed styles, downloaded R evaluation and JSON values,
  invalid contrast, reset, phone preview and mobile document overflow. It also
  fits the generated app's unchanged synthetic primary model (estimate 0.24).
- The existing generated-app browser smoke covers exports, workbook round trips,
  changed uploads, equivalence, filtering and reset.
- The early Analyze control remains disabled until the dynamically inserted
  z-score filter is bound. A regression check covers the silent readiness guard;
  models and filtering rules are unchanged.

The PR includes screenshots of the helper, its generated app and a dark palette.
