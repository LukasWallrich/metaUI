# metaUI (development version)

- Odds ratios and risk ratios are supported effect-size scales (`es_type = "OR"` or `"RR"`). `prepare_data()` takes log ratios with `variance_scale = "log"`, or 2x2 counts through the new `counts` argument, converted with `metafor::escalc()` and its defaults (0.5 added to every cell of a table with a zero cell; tables without events in both groups kept). Models are fitted to the log ratios, so estimates match `metafor` on the log scale; estimates, confidence, prediction and 90% intervals are exponentiated for display, with 1 as no effect, and the Summary, Forest plot, Moderation, funnel, distribution and forest download use log axes. Moderator slopes, heterogeneity variances and Egger's test stay on the log scale and are labelled so. The equivalence bound is entered as a ratio *x* and assessed against 1/*x* and *x* ("Exceeds upper/lower bound"). P-curve and z-curve tests use the Wald *z* of the log ratios; p-curve effect estimation stays limited to standardized mean differences. Count columns are editable in downloaded workbooks, and uploads recompute the effects from them. The Bayesian heterogeneity prior defaults to half-normal(0.5) on the log scale.
- The forest plot download no longer drops the top summary diamond (or the bottom study's shading), which fell just outside the plot's range.

- Small-sample corrections in the default models. Robust variance estimation uses Tipton's (2015) correction (`robumeta::robu(small = TRUE)`) and flags its row as unreliable below 4 small-sample degrees of freedom; the multilevel model's *t* tests use containment degrees of freedom (`dfs = "contain"`), which count studies rather than effects. With few studies the intervals are wider than before; with many they barely change. Equivalence assessments inherit the wider 90% intervals.
- Moderator tests allow for dependent effects. Each moderator is added to the primary model where that is an effect-level `metafor::rma.mv` model (a CHE model, say), and otherwise to the default multilevel model (REML, no longer ML); tests and level intervals are cluster-robust by study (CR2 with small-sample degrees of freedom, via clubSandwich, now a dependency of generated apps). The overall test is clubSandwich's HTZ test for any number of categories, computed by an equivalent fast route (33 categories on 2,628 effects in under a second rather than over 15 minutes and 9 GB). The tab says which model a test extends, and the note says that the tests are exploratory and unadjusted for multiple testing.
- Uploads re-derive what can be derived. The workbook's new first sheet, `instructions`, marks each dataset column as Edit, Derived, Fixed or Other, and the dataset sheet lists the editable columns first. On upload, the effect size and variance on the fitting scale, the SE and the z-score are recomputed from the supplied effect and variance; fixed columns and empty effect IDs are filled in for new rows; new categories are added to their filter and selected. Upload errors name the column and workbook rows, and a file that is not a workbook is reported as such.
- Selection links keep working after the data are updated. A link made for earlier data is applied as far as it still fits: categories that are gone are dropped, ranges are trimmed to the new data, and a banner says that the data have changed and what was adjusted. Links for the current data are checked as strictly as before.
- The estimates table marks the conditional PET-PEESE estimate (Stanley & Doucouliagos, 2014): PEESE when PET's intercept differs from zero at the 10% level, one-sided in the expected direction or two-sided without one (Stanley, 2017), and PET otherwise; a note gives PET's p-value. Both rows stay. When trim-and-fill moves the estimate further from zero (smaller studies show weaker effects), a note says so; Methods says that the side to fill comes from Egger's test, not from the expected direction. Downloads gain `pet_intercept_p`, `pet_peese_selected` and `trimfill_away_from_zero`. Reader-facing text no longer says "declared" ("the direction the authors expected", "chosen by the authors").
- P-curve is drawn for correlations too, on Fisher's z; only its effect estimate (in d) stays limited to standardized mean differences.
- `prepare_data(na.rm = TRUE)` drops rows with a missing value in a filter, or in the p-value or sample size when these are given, including categorical filters (which then have no `(Missing)` level); other columns of the data no longer count. `arrange_filters = "alphabetical"` sorts by label.
- Fixes from a pre-release review. Filter labels with apostrophes, quotes, brackets or other symbols no longer break the generated app or the Moderation table; labels that would give the same input ID, repeated labels, backticks and numeric filters without values are refused with a message. Source labels in the Sample table are shown as text, not HTML. With one effect selected, Heterogeneity says it needs two instead of showing an error, and a numeric filter without values in the selection is summarised as "No values". The Summary warns about fewer than ten studies in apps without a declared primary model too. On static sites, selection links skip the full-data preview again.

- Equivalence uses each model's 90% interval: an interval inside the bounds is the usual pair of one-sided tests at .05 (TOST). The panel says why it differs from the 95% intervals elsewhere. Model specifications gain optional `LCL90` and `UCL90` columns (P-uniform* repeats its profile likelihood at 90%); a model without them is not assessed, and downloads hold `LCL90` and `UCL90` with the other estimates. "New selection" counts have thousands separators.

- The Summary draws every model as a diamond across its 95% CI, the primary one in the accent colour; the prediction interval is no longer drawn (its sentence above the table stays). Moderator levels whose CI includes zero have outlined diamonds. Heterogeneity names its source model without referring to the prediction interval.
- The sidebar opens with a "Selection" heading and the selection status under it ("Results use 2,628 of 2,628 effects (646 studies)", with thousands separators), set off by a rule from the "Filters" that change it. The equivalence bound is a short entry on its label's line; its last column reads "90% interval lies within ±".
- Sample: study and source rows give the range of their effect sizes ("−0.080 to 0.040"), in place of the note sending readers to the Forest plot; only the table scrolls sideways, not its search and paging. References written by the authors get APA italics from their BibTeX entry (an article's journal and volume, a book's or thesis's title) and linked addresses; other entry types stay as written.
- Forest plot: order by study name, largest or smallest estimate, and go to any page from a menu. The PDF download holds every study, `max_forest_plot_rows` effects to a page (default 60, whole studies, one shared axis); the PNG download is the page on screen.
- Moderation: the finding sits over its table at the table's width and keeps its test on one line; a numeric moderator's plot has its key beneath it. Tabs load in the background in tab order, with shorter pauses, so Moderation is ready sooner.
- Publication bias: the p-curve and z-curve notes sit in their own sections, and apps without p-curve no longer say so. The z-curve threshold reads "z > 1.96, p < .05".
- The list of effects beyond 1.5 IQR gives each effect's *N* and 95% CI. Upload help says what an upload needs: the sheets and column names of a downloaded workbook.

- Heterogeneity on Outlier diagnostics comes from the primary model when it has study and within-study components, and says which model it comes from. Slider ranges can be typed: press the range beside a filter's name. On phones the funnel shows every other tick when its labels would crowd, and the z-curve gives its threshold statistic a line of its own.
- Forest plot: the switch reads "Effects within studies: hidden | shown", Find a study says on which page the study is, the note on effect numbers appears only while effects are shown, and on phones the downloads follow the table.
- Methods says in plain words which question each default model answers before giving its specification, and states the one assumed within-study correlation. Robust variance estimation now uses that declared correlation (0.6 by default) instead of robumeta's default of .8, so the app makes one assumption about it; on the Diversity data the estimate and SE agree to four significant digits. A numeric moderator's slope has a 95% CI, and the count beside "Include missing values" follows the other filters as they are set.
- Moderation's dashed line is the primary estimate, as on the Summary; the funnel says that its line can differ.
- Sample breakdowns keep "(Missing)" in a row of its own and list an author's "Other" level like any other; levels the author put in order (such as hours, days, weeks) keep that order in the breakdowns and in Moderation.
- About says in plain words how many effects were left out when the app was built, and why. The page has one top-level heading, pickers name their filter for screen readers, and plots have descriptive alternative text.
- The forest plot download matches the screen: the effect-size name on the axis, estimates and intervals beside the rows, model summaries first and effects named by their study.
- Downloads are named after the app's data and date, so files from different apps no longer overwrite each other; the data download says what it holds. The equivalence table defines its labels. The z-curve note explains how its replicability rate relates to p-curve's power estimate.
- Moderator tests leave out effects without a value for the moderator, including a kept "(Missing)" category, and say how many entered the test. Before, "(Missing)" was tested as a level of its own. Moderation warns when fewer than 10 studies enter a test.
- Narrow selections give plain reasons instead of R's messages (one study, singular fits, too few significant results for p-curve or z-curve). Model footnotes no longer repeat or contradict each other, and every unestimated model reads "Not estimated".
- The prediction-interval sentence no longer claims that true effects vary, and is left out below three studies. Equivalence counts only the models that gave an interval. The z-curve verdict is held back when its own warning applies, and the funnel caption tells the estimate line from the sloping 95% lines.
- Shared links: Copy link in a static export copies the page's address rather than the inner frame's, a damaged or outdated link says so in a banner above the results, a link being applied is announced at once, and Copy link warns when the filters on screen are not yet applied. Links are short and readable: `?metaui=a63ba039&country=Greece,Kuwait` keeps two countries, and `country!=Kuwait` keeps all but one, for the data version a63ba039. Parameters a site adds, such as tracking tags, are ignored. A static export takes the app's title, and its saved preview greys out the tabs until the app has loaded.
- Pickers open downwards as intended (the option was misspelt), negative slider values use a true minus sign, *p* values in tables drop the leading zero, *F* values keep two decimals, and empty table searches read in the app's own words.

- Generated apps open on results: `generate_shiny()` fits the default analysis at build and saves it as `opening_results.rds`, so no reader waits and no reader triggers a fit. Any later edit to the app's analysis files retires the saved results, and the app then fits on opening. The button is now **Update results**, quiet until filters change.
- The forest plot opens on one row per study: a study with several effects shows its combined study-level effect (filled square) and opens to show them, labelled by what differs within the study. It can show every effect and sort by estimate. Downloads are unchanged.
- P-curve and z-curve open with a plain reading of their tests. The p-curve is drawn in the shared plot style from the same values, and z-curve rates appear as percentages.
- New `options = list(intro = ...)` adds a short introduction under the title.
- A diamond now always spans a 95% CI, on the Summary, on moderator levels and in the forest plot's PDF and PNG downloads. Moderator rows stack their effects so that no dot covers another, and funnel labels move clear of the points.
- Categorical moderator tables open on the level estimates, on an axis that fits them, with a button to show the individual effects.
- Categorical moderator levels with many effects (25 or more on phones, 100 or more on larger screens) show a small histogram instead of stacked dots, which keeps those rows short and the page responsive.
- Correlations are shown as *r* everywhere readers see effect sizes: moderator tables and plots, histograms, the effects table and the first screen. The funnel keeps its straight limits on the Fisher z scale with its axis labelled in *r*. Slopes, variances and Egger's test stay on Fisher z and say so.
- Estimates show three decimals where an interval is narrower than 0.05.
- The forest plot shows any number of effects, 50 studies to a page drawn on the server. A declared primary model leads its summaries and sets the reference line.
- With `study_info`, readers can download the references for their selection as text, BibTeX or a table. `study_info` gains optional `reference` and `bibtex` columns.
- Selection links save only the filters that narrow the data, so they stay short with many filters.
- Custom models can name their interval in an `interval_type` column.
- The Summary gives the primary analysis more weight: its row is tinted, and the other estimators sit under it, grouped by level of fit. Results keep a readable width on very wide windows, and filters a reader has narrowed stand out from untouched ones.
- After each analysis, the other tabs are drawn in the background, one at a time, so they open at once. A tab opened before it is ready shows one message, never empty headings or the previous selection's results. Bayesian and effect-computation tabs still wait to be opened. A tab counts as ready only once the server has answered for it, and screen readers announce the message.
- Saved opening results no longer blank for a moment when the app connects, and the opening page no longer repeats its strip of effects in hidden placeholders.
- The selection count and its details sit at the top of the filters, above the results. The Summary no longer restates the sign and interval of the headline estimate, and moderator findings read as one sentence.
- The Sample tab's table can show one row per effect or per study. Study rows keep values their effects share and show ranges otherwise; they never average effect sizes. The Source column is left out when study names already give it, whole numbers show no decimals, and the sample-size column is gone.
- Reference downloads and "Copy link" (which replaces "Link to this selection") look like downloads and actions, with icons. Copy link works on plain http pages too.
- A model that fails to fit now says it could not be estimated, with the reason; "not estimated" is kept for methods that do not apply. A failed moderator model no longer shows the optimizer's raw error.
- Controls that do the same job look the same. Every choice of view (rows per effect or study, order, individual effects) is one labelled switch showing the current option. Update and Reset are both buttons; downloads and Copy link are outlined buttons with icons; the Sample table pages with the same buttons as the forest plot, from its top as well as its foot.
- The Sample breakdown no longer counts an existing "Other" category twice, so long categorical breakdowns now add up to the selected effects.
- Shorter, plainer text: the Summary names the primary model in its table rather than in a line above it; Selection details lists only filters that excluded effects; a pending selection says to update; the numeric moderator's intercept says where it is predicted; p-curve explains its tests; the effects table's p column is "Reported p". The repeated note on outlined missing-value bars and the generic About sentence are gone.
- A tab drawn in the background now waits for outputs nested in others (such as the moderator table) before it counts as ready.
- Fixes from a reader review. An empty selection can no longer be applied, and a tab with nothing selected says so once instead of showing errors. Failed or inapplicable models give a plain reason (the package's own text stays in downloads), and so do model warnings, damaged links and invalid equivalence bounds. The numeric moderator's table gives the effect at the mean of the moderator, with its interval, instead of the intercept at zero, and says when effects without a value drop out. Outlier diagnostics lists the effects beyond 1.5 IQR. A banner above the tabs marks results that no longer match the filters, or a link or upload being applied. Long pickers open downwards. The z-curve's sample-size warning says what it means, the curve says what it is fitted to, and tests beyond the plot are counted in the axis title. Publication bias warns that a non-significant test does not show there is no bias, and both it and the Summary warn when there are fewer than ten studies. The Sample tab defines sources, studies and effects, links to its table, lists what an "Other" row holds and counts empty categories instead of listing them; study rows round decimals like effect rows. Heterogeneity variances on Fisher z say so; P-curve is one line for data it does not apply to; z-score descriptions agree; the forest download limit sits by its buttons; data-tool notes about Bayesian models appear only with them. While the reader has not chosen a moderator, Moderation opens on one that varies in the selection.
- A fit whose standard errors cannot all be computed ("NaNs produced") is reported as could not be estimated: it shows no numbers and is left out of the Summary's range. Downloads keep its values with status `failed` and a reason that says not to interpret them.
- Intercept-only metafor models report a 95% prediction interval (new `PI_LCL` and `PI_UCL` columns in the downloaded summary), and the Summary states the primary model's under the headline.
- The Sample table can show one row per source as well as per study or effect, when sources are declared. Custom models can carry a `description`, which Methods shows.
- Moderator tests are saved at build for the default selection and kept for the session, so Moderation opens and switches at once on the default selection.
- The forest plot has a Find a study box that jumps to the study's page and marks it.
- Range filters end at the data's own range, to the step, rather than at the next round number. The Egger table no longer shows the regression's residual dispersion, which read as heterogeneity. Z-curve warnings come before its reading.
- Dark mode: with a dark system setting (or `data-theme="dark"`), apps in the default design switch to dark colours, and the server draws their plots in a matching dark palette. Downloads and author designs keep their own colours. Range sliders are keyboard and screen-reader accessible, one named slider per handle.
- Effect-level rows of the Summary table all count effects, so RVE no longer reads as a different sample; downloads keep each model's own *k*. The Sample table's study cells are built as text, which makes the tab draw about twice as fast, and apps draw one small plot at start so the first reader's plots do not wait. Stylesheet and script addresses carry a version, so browsers do not reuse an outdated copy.
- Apps with `fit_cache_entries = 0`, the default for custom model code, no longer save or reuse build-time fits: they fit afresh when opened.
- A moderator with one value in the selection now says it cannot be tested, instead of failing. Declaring RVE as primary now reports its *k* as studies. Static previews state the actual default selection.

* Redesigned the generated apps' reader interface. The Summary now leads with one
  sentence stating the primary estimate. A single table draws every model's interval on
  a shared axis, replacing the separate plot and table, and gives reasons in the row
  when a model was not estimated. The forest plot and categorical moderators use the
  same table form; the forest plot's PDF and PNG downloads are unchanged in content.
  Effect distributions are histograms rather than violin plots, and numeric moderator
  plots now draw the fitted multilevel model rather than an unweighted line. The first
  screen shows the effects as a dot histogram without fitting any model. An empty
  selection is explained in the page instead of a dialog.
* Apps bundle the open-licensed Source Sans 3 and Source Serif 4 fonts (about 200 KB).
  Plots use Source Sans where `ragg` and `systemfonts` are installed. `app_design()`
  now defaults to these fonts and a forest green accent, and gains `font = "source"`.
  The earlier palette remains available as `accent = "#096B74"`.
* Author-enabled p-curve effect estimates now report a failed fit, with the reason,
  when the estimate falls below zero. Previously such an estimate was reported
  with a sign opposite to the declared direction.
* Publish apps without a Shiny server. `generate_shiny(static_folder = ...)` and
  the JSON `static_folder` field write a static website that runs R in the
  reader's browser (shinylive/webR); `export_static()` re-exports an app after
  hand edits in the editable app folder. With `overwrite = TRUE`, the existing
  static site folder is deleted entirely and rewritten. Readers see the authors' full-data results at once while R loads.
  Selection links, downloads and the Summary plot work in the static site, and
  `include_sources = TRUE` bundles the source code of copyleft components for
  archival deposits. Requires the suggested shinylive package. Every visitor
  downloads the full dataset and app code, so publish only shareable material.
* Generated apps work on phones, on a Shiny server and as static sites. The
  filters collapse behind a "Show filters" button so that results appear
  near the top of the page; tables with four or more columns become one labelled card per row
  instead of scrolling sideways; wide plots scale to fit, and the forest plot
  wraps study labels. Static sites also show the precomputed preview at phone
  width, including when filters use selection boxes, which previously prevented
  the preview from being computed.
* `prepare_data()` decodes HTML escapes such as `&amp;` in text and factor columns, which
  apps previously showed verbatim (for example, in study labels), and reports
  the columns changed. Study details are matched to the decoded study IDs.
* Forest plots taller than about a screen repeat the effect-size axis at the top.
* Generated apps use metafor for trim-and-fill, the funnel plot, Egger's test and
  the Dannheim source-reference fit. Results are unchanged. The meta package is
  needed only when author model code uses it.
* Generated apps can download an R script or Quarto file that reproduces the
  applied selection (filters, missing-value choices, effect-size computation and
  equivalence bound) and its model rows from the workbook of the same analysis,
  including uploaded data. The code copies the app's `models.R` (or, if the file
  no longer matches the loaded models, their table) and the fitting functions it
  uses, runs without metaUI, records the package versions the app
  used (with an optional, commented-out groundhog call) and compares its results
  with the app's. Plots and diagnostic tabs are not reproduced.
* Uploading a workbook now restores its equivalence bound, as selection links
  already did. Workbooks from empty selections also record the bound.
* metaUI no longer depends on forcats, and generated apps no longer need psych;
  base R replaces both. The R Markdown skeleton's example recode now treats
  `-999` as missing (shown as `(Missing)`) instead of a level named "NA".

# metaUI 0.2.0

This release consolidates all changes since 0.1.1, including the unreleased
0.1.2 development versions.

## Breaking changes

* Packages used only by generated apps (DT, ggplot2, meta, plotly, psych,
  puniform, readxl, robumeta, shinycssloaders, shinyjs, weightr, writexl and
  zcurve) moved from Imports to Suggests. `generate_shiny()` names any that are
  missing, with the command to install them.
* `generate_shiny()` and `build_app()` refuse a nonempty output folder unless
  `overwrite = TRUE`, so hand edits to a generated app are not overwritten by
  accident. `overwrite = TRUE` replaces all generated files, including the
  editable `www/metaui.css` and `www/metaui.js`; build into a fresh folder when
  hand edits must be kept.
* Generated apps check their dependencies at startup but no longer install
  packages; `dependencies.csv` records the versions used for the build.
* Correlations must declare their variance scale, and directional bias analyses an
  explicit direction. COR is fitted via Fisher's z with a delta-method variance,
  ZCOR is not transformed twice, and summaries are back-transformed once.
* Multilevel models nest effects within genuine per-effect IDs; display labels may
  now repeat. Summed sample sizes are no longer inferred.
* Generated apps no longer use shinyBS or waffle: filter popups became inline help
  and the waffle chart became a categorical count bar chart.

## New features

* Study details in the list of effect sizes. `generate_shiny(study_info = ...)`
  adds a disclosure button to each study; it opens a row with an APA-style
  reference, a button that copies it, the DOI link and, where permitted, the
  abstract. `fetch_study_metadata()` looks up DOIs in Crossref while authoring,
  uses OpenAlex for licences and abstracts that Crossref lacks, reports DOIs it
  could not match and can cache results. Abstracts are kept when the article is
  published under a Creative Commons licence (including ND and NC terms), CC0 or
  the Public Domain Mark; `abstracts = "exclude_nc"` drops NC licences for
  commercial apps, and `abstracts = "all"` keeps every abstract found. In schema
  version 2 configurations, `study_info` is a CSV path or a lookup object
  (`{"cache": "study-metadata.csv"}`) that `build_app()` runs into that cache.
  Generated apps never query external services.

* The bundled Coles et al. (2019) facial-feedback data (`inst/extdata/coles2019.csv`)
  include the source's outcome-type column `paq` (affective judgement versus
  emotional experience), so apps can filter by it.

* `build_app()` creates apps headlessly from a JSON configuration with a versioned
  schema (version 2 adds computations, Bayesian analysis and design; version 1
  behaviour is retained).
* `design_app()` previews colours, fonts, type size, spacing and corners and exports
  R/JSON settings; `app_design()` validates settings and contrast. Pass them via
  `generate_shiny(design = ...)` or the JSON `design` field. Apps built without a
  design keep the default appearance.
* Authors can declare one `primary_model`. Apps show it first as the authors'
  primary analysis, label other rows as explorations, and report why it is
  unavailable instead of substituting another model.
* Author-defined effect-size computations, with explicit scale and variance
  contracts, consistent row selection, comparisons, provenance and workbook
  restoration.
* Opt-in Bayesian normal-normal analysis (requires the suggested bayesmeta
  package): posterior medians, central credible intervals, disclosed priors and
  heterogeneity-prior sensitivity.
* Readers can share applied selections through build-specific, validated URLs.
  Uploaded datasets remain workbook-only.
* Practical-equivalence assessment of intervals against a reader-chosen symmetric
  bound (no default). Changing the bound does not refit models; downloads record
  the assessment and bound.
* Forest plots show observed effects on the displayed scale, compare available
  multilevel/RVE summary intervals, and export to PDF, PNG and CSV.
* `generate_shiny()` gained `options` (Shiny theme, forest-plot row limit,
  checkbox/selection-list threshold); `prepare_data()` gained `arrange_filters`.
  Apps can be created without any filters or moderators.

## Generated app interface

* The interface is reorganised around selecting evidence, running models and
  reading results. A status bar leads with selected and excluded counts, filter
  changes not yet analysed are flagged until Analyze data is clicked, and result
  tabs say when no analysis has been run. Routine instructions and repeated
  explanations were cut or moved into disclosures; scientific warnings,
  provenance and model distinctions remain visible.
* Models that were not estimated are listed with their reasons. A collapsible note
  derives scale, declared direction and aggregation assumptions from the data
  contract. Model applicability, failures, warnings and fitting values are
  reported, and z-curve refusals and warnings are shown.
* Generated apps include editable `www/metaui.css` and `www/metaui.js`: responsive
  tables and plots, WCAG AA text contrast, visible keyboard focus, and a skip link
  to results.
* Uploads are validated, kept in session state and clear cached results. Downloads
  contain the full current data, applied selections, selected summary and
  provenance, and wait for pending restorations.
* Slider bounds round outward to a round tick interval so default bounds retain all
  eligible rows and end labels no longer overlap.
* The effect-size list rounds effect sizes to three decimals and other numeric
  columns to two, and shows URL/DOI columns as links. A "(Missing)" level appears
  only where a filter has missing values.

## Bug fixes

* Corrected the random-intercept specification in `rma.mv()` (now with
  `sparse = TRUE`), REML heterogeneity output, and Egger residual variance.
  Meaningless multilevel tau² is no longer shown; singleton-study variance splits
  are reported as unidentified.
* Optional p and N columns no longer drop rows under default preparation; required
  inputs are validated, and excluded rows and derivations are reported.
* Categorical filter levels containing a double quote no longer stop the app from
  starting.
* A non-converging random-effects model behind the funnel plot and Egger's test
  now gives a clear message instead of a raw error.
* The outlier violin plot is standardised against the built dataset and no longer
  fails when no outliers are drawn; moderation slopes are no longer rounded to
  "-0.00"; all moderation-table rows are shown.
* Correct the tutorial's Wald p calculation and Fisher-z labels.
* Fixed describing moderators where an "Other" category already existed, and
  checking that filters are factors or numeric (#28).

## Performance

* P-curve deduplicates identical noncentrality solves without changing results.
* Compatible multilevel/RVE fits are reused across diagnostics, and a session-local
  three-entry cache speeds repeated selections. Custom models default to no
  caching; `options$fit_cache_entries = 0` disables it. Measurements are in
  [validation/performance-optimisation.md](https://github.com/LukasWallrich/metaUI/blob/main/validation/performance-optimisation.md)
  in the source repository.

## Testing

* New examples: the CC-BY Dannheim mental-health pool with a source-reference
  REML/Hartung-Knapp model, and a synthetic correlation example with an
  independent Fisher-z reference fit.
* Independent statistical references, headless server tests and a generated-app
  browser test; CI checks Linux (current and previous R), macOS and Windows.

# metaUI 0.1.1

* Fixed issue where k was not displayed for moderators with spaces in names
* Simplified implementation of `generate_shiny()` - it is now always saved, either to provided path or to temporary files, and launched from file. This should increase robustness. Also, removed unnecessary `app.R` so that `globals.R` does not need to be called explicitly.
* Added a `NEWS.md` file to track changes to the package.
* Added explicit option to include or exclude NAs on filters and report NA share in Sample descriptives
* Added `Reset` button to reset all filters
* Added option to add popups to filter, by specifying `filter_popups` in `generate_shiny()`
