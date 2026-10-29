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

- Full local R CMD check passed with zero errors and warnings, including the
  Bayesian tests after installing the optional dependency. One incoming-check
  note reports existing p-curve URLs returning HTTP 406 from this machine.
- Focused tests cover R/JSON settings round trips, config builds, validation before
  writes, preserving input data, theme removal on rebuild, invalid-export recovery
  and label/background colours on dark forest plots.
- `tools/design-smoke.cjs` checks palette/font/size/spacing/corner controls, preview
  versus generated-app computed styles, downloaded R evaluation and JSON values,
  invalid contrast, reset, phone preview and mobile document overflow. It also
  fits the generated app's unchanged synthetic primary model (estimate 0.24).
- `tools/design-dark-smoke.cjs` exercises real dark dialogs, DataTables headers,
  pagination and sort indicators, selectize and numeric inputs, and forest output.
  `tools/design-browser.cjs` owns the helper/light/dark server lifecycle; CI now
  generates these fixtures and runs both design browser suites.
- The existing generated-app browser smoke covers exports, workbook round trips,
  changed uploads, equivalence, filtering and reset.
- The early Analyze control remains disabled until the dynamically inserted
  z-score filter is bound. Focused source tests cover the silent readiness guard; browser tests wait
  for readiness and do not assert the pre-bind state. Models and filtering rules
  are unchanged.

The PR includes screenshots of the helper, its generated app and a dark palette.

## Review corrections

Opus and Argon both requested changes after reproducing unreadable dark-theme
controls. The override now covers modal and notification surfaces, DataTables
headers/sorting/paging, native/selectize inputs, data-tool buttons and stale/warning states. Selectors
also override DataTables styles loaded after the app stylesheet. Regression tests
first reproduced the white dialog/light text failure and the paging failure,
then passed after the corrections. Forest intervals use the chosen text colour
and are checked against the panel at the graphics contrast threshold.

Both reviewers confirmed that font stacks are deployable without author-machine
font files. System sans now uses `system-ui` and platform fallbacks. The helper
explains device-dependent substitution; it does not promise identical fonts on
all devices. An explicit default design differs from the legacy `design = NULL`
look. Manual colour edits update the preset selector, and no empty settings record
is retained when rebuilding without a design.

The failing cross-platform CI checks came from a literal bullet in the base
redesign's R template. The generated code now carries a Unicode escape, keeping
package source ASCII while preserving the displayed separator. That fix is in
both PR branches; PR #40's four-platform and browser checks passed on GitHub.
