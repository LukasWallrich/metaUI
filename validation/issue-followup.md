# Open-issue follow-up, 6 October 2026

This work is stacked on #36 (reader UX), itself stacked on #35 (correctness and
performance). It does not merge, release, deploy publicly, or contact anyone.
Read-only Opus 5.5 consultation reviewed every open issue and the larger additions;
a second consultation agreed on the bounded interval-assessment contract below.

## Completed in this follow-up

| Issue | Result |
|---|---|
| #31 | Remove shinyBS. Expandable inline help preserves author HTML, tables and clickable links. Native buttons support keyboard and touch; Escape closes open panels. The help panel sits outside the input label. No Stack Overflow snippet is copied. #34 is superseded by this implementation and should not be merged. |
| #29 | Repeat the selected sample summary with two distinct output IDs backed by one reactive calculation; keep the welcome dialog. |
| #30 | Oversized forest selections show an HTML message and create no plot output. The exact limit still renders; the plot and all exports share the guard. Fresh shinyapps.io deployment remains a hosting-specific acceptance check. |
| #14 | ggplot forest with fixed row spacing, selected effects grouped by study, multilevel/RVE summary intervals, PDF/PNG/CSV exports, and r display for correlations. Shrinkage was considered and left out: observed effects and model predictions answer different questions; any future overlay must be explicitly labelled and separately validated. |
| #27 | Reader-chosen symmetric smallest effect size of interest on the displayed scale, with no arbitrary default. Classify existing intervals and record the bound, threshold and assessment in workbook exports. |
| #32 | Explain interactive console blocking, build-only generation, returned folder/app object, and separate launch. |
| #23 | Installed, runnable synthetic correlation example with an independent metafor reference fit. #35 already provides explicit COR/ZCOR conversions, eligibility and back-transformation tests. Odds ratios remain unsupported. |
| #33 / #3 | Assess the living-meta-analysis tool and collect related-tool references in README. Fix the Allbritton spelling and broken Markdown link. |

The mobile browser check also found that Sample-tab chart images could temporarily
retain desktop width after resize. They now fit their columns while Shiny redraws.

## Practical-equivalence contract

This is an assessment of existing model intervals, without refitting or altering
model inference. For bound Δ > 0, containment requires **−Δ < LCL and UCL < Δ**;
exceeds-positive requires LCL > Δ, exceeds-negative UCL < −Δ; equality and all
other overlaps are inconclusive. Failed/unsupported rows or missing intervals
are not assessed. With no bound entered, only interval thresholds are reported.
The threshold is max(abs(LCL), abs(UCL)): containment holds for a bound strictly
larger than that value. For r, Δ must be strictly between 0 and 1. Comparing
back-transformed r intervals is equivalent to transforming bounds to Fisher z.

Unchanged default model specifications are recognised by their complete spec,
not just their name. Their reported 95% CI containment corresponds to TOST at
**nominal α = .025 per one-sided test**, conditional on the interval assumptions;
this does not claim conventional α = .05 TOST. Custom intervals have unknown
coverage. Bias-adjusted estimates describe their own assumptions. The assessment
concerns the average effect, not the range of individual true effects.

The multilevel model retains residual degrees of freedom and RVE retains
`small = FALSE`, as explicitly tested in #35. Their intervals can be too narrow.
This limitation appears beside every assessment. Opus and Codex agreed to leave
inferential corrections to a separate scientific-contract PR rather than silently
change all currently reviewed estimates and intervals.

References: [Lakens on equivalence testing](https://lakens.github.io/statistical_inferences/09-equivalencetest.html),
[metafor rma.mv inference](https://wviechtb.github.io/metafor/reference/rma.mv.html),
[robumeta documentation](https://rdrr.io/cran/robumeta/man/robu.html).

## Existing stack coverage

#25: #35 keeps the hidden workbook download handler active for JavaScript-triggered
downloads. A real browser download/re-upload is exercised by repository CI; this
follow-up verifies downloads with the additional equivalence sheet.

#13: p-curve magnitude estimates remain removed from the default ensemble. A
magnitude reported as Cohen's d must not be interpreted as Fisher z or log-OR,
or assigned the sign of another model. P-curve remains an exploratory diagnostic
with explicitly declared direction. Closing the old sign issue does not imply
reintroducing those estimates.

#26: release preparation is completed by the next stack layer: version 0.2.0,
runnable examples, dependency/licensing review, offline vignettes, announcement
draft and local manual/as-CRAN checks. See [release preparation](release-0.2.0.md).
Public submission and release publication remain author actions.

## Completed analysis features in the next stack layer

**#9 Bayesian model:** implemented opt-in `bayesmeta` study-level normal-normal
analysis with a flat mean prior, half-normal heterogeneity prior on the fitting
scale, posterior median, central 95% credible intervals and half/double prior
sensitivity. Generated apps record priors/dependency versions. Independent
normal-mixture quadrature verifies quantiles. The study limit is benchmarked.

**#1 effect-size calculations:** implemented up to five author-supplied alternatives,
each with its own effect/variance/scale/justification contract, preserved effect IDs,
identical primary-kept rows, applied selector and configured multilevel/RVE comparison.
Cache keys, stale-state detection, workbook input validation and restoration include
the computation. A synthetic example compares two correlation-variance rules.

**#19 URL filters:** implemented build-specific versioned, validated query state for
applied selections, computation and equivalence bound. Categories retain punctuation
and Unicode; invalid links display a notice and do not automatically analyse.
Restoration awaits browser acknowledgement and times out safely. Workbook downloads
retain the data behind the displayed analysis. Uploaded data remain workbook-only.

**Inference correction:** independently check `dfs = "contain"` for multilevel
fits and small-sample-corrected RVE, including low-df refusals, before changing
the existing scientific contract. This is separate from the interval display.

## Related-project assessment

[Allbritton et al.'s General Tool for Living Meta-Analysis](https://github.com/davidallbritton/Breathing_Life_into_MetaAnalysis)
provides live-data refresh, model/moderator controls, optional Bayesian analyses,
and a code generator. metaUI already offers dataset upload/download, filtering,
moderation and model comparison, with author-owned editable generated R code.
The clearest useful transfers are exportable forest plots (implemented here),
validated shareable selections, and opt-in Bayesian estimation (since implemented in metaUI 0.2.0).
Live remote refresh needs source authentication, provenance, validation and
failure recovery; it should not silently alter the dataset behind published results.

MIT code can be included with its notice under metaUI's GPL terms; this assessment
does not change the package licence. Collaboration and relicensing are decisions
for the authors, not prerequisites for these improvements. Related tools are
references and inspiration; no code was copied from restricted sources.

## Validation

- Source suite: 284 assertions passed, no failures, warnings or skips.
- `R CMD check --no-manual`: Status OK; installed tests skip only the existing
  source-tree parity check, which passes in the source suite.
- Repository Chromium smoke: startup, analysis/cache reuse, keyboard inline help,
  both sample summaries, forest rendering/exports, bound changes, workbook
  download/re-upload, restored category/missing-value selections, filtering/reset.
- A separate row-limit browser fixture passes 13 checks, including no plot/export
  links above the limit, rendering at the exact boundary, PDF/PNG/CSV downloads,
  workbook download and no page overflow at 390px. Workbook sheet values and forest
  CSV membership are verified independently after downloading.
- Opus 5.5 review agreed on the scoped interval contract; its plain-text escaping
  finding and empty-selection improvements were applied. Unicode/HTML/plain-text
  escaping, strict boundaries and unchanged fit state are covered by regressions.

A private review instance with the synthetic 16-effect fixture (8-row forest
limit) was used for manual checks; the production default remains 200.
No screenshots or user data are uploaded to GitHub. Hosted CI will repeat the
package/platform and browser checks for the stacked PR.
