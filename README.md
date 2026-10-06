# metaUI <img src="man/figures/logo.png" align="right" style="height:200px; padding: 10px;" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/lukaswallrich/metaUI/workflows/R-CMD-check/badge.svg)](https://github.com/lukaswallrich/metaUI/actions)
[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/LukasWallrich/metaUI)
<!-- badges: end -->

The metaUI package allows users to quickly create powerful and customisable Shiny web apps that allow readers to explore a meta-analytic dataset in depth. They can explore the impact of different analytical choices, see results for subgroups of particular interest, and even modify and update the underlying dataset.

The motivation for the package came from the fact that meta-analyses are based on rich datasets that can be analyzed in numerous ways, and it is unlikely that authors and readers will always agree on the “best ways” to analyze the data. Whether it comes to the choice of model (e.g., random versus fixed effects), the methods for assessing or adjusting for publication bias (e.g., z-curve, p-curve, PET/PEESE etc), or the moderators to be considered, disagreements are likely to arise. This can lead to the inclusion of lengthy robustness checks and alternative analyses that are time-consuming and difficult to digest. metaUI, as an R package that supports researchers in creating interactive Shiny web apps can help - these apps allow readers (and reviewers) to explore meta-analytic datasets in a variety of different ways. Apart from allowing readers (and reviewers) to assess the robustness and trustworthiness of results more comprehensively, metaUI apps allow users to assess the results that are most relevant to them, such as by filtering the dataset to focus on a specific group of participants, region, outcome variable, or research method. With the opportunity for users to download the dataset used and to upload alternatives, it will also facilitate the updating of meta-analyses.

The idea came from researchers who have created similar web apps for their meta-analyses that have been well received, yet they require substantial time investment and advanced coding skills to create. With metaUI, researchers can get a working app very swiftly – while they still have the flexibility to tailor the display in line with their interests and requirements. This branch is a tested development candidate with scientific and licensing limits documented below; feedback and contributions are welcome.


## Installation

You can install the development version of metaUI from [GitHub](https://github.com/) with:

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("LukasWallrich/metaUI")
```

## Getting started

The best way to get started in RStudio is with the metaUI template - so in RStudio, go to `File -> New File -> R Markdown` then select `From Template` and `Create a metaUI Shiny app`. To see the template in the most readable format, select `Visual` just above the file that has opened to enter the Visual Editor. Then you can follow the step-by-step instructions.

Outside R Studio, you can run `file.edit(system.file("rmarkdown/templates/create-a-metaui-shiny-app/skeleton/skeleton.Rmd", package = "metaUI"))` to open the template file and then take it from there.

The [`Getting started` vignette](https://lukaswallrich.github.io/metaUI/articles/getting_started.html) provides a step-by-step example ... or you may want to watch this [video tutorial](https://www.youtube.com/watch?v=iaTMFzWfCe0&ab_channel=ESMARConf) for a step-by-step walk-through.

## Sources

Much of the package code was based on two previous interactive meta-analyses apps:

 - Röseler, L., Körner, R., & Schütz, A. (2021). Dynamic Meta-Analysis. Retrieved from https://osf.io/ns65r/, Shiny app accessible [here](https://metaanalyses.shinyapps.io/bodypositions/)
 - Röseler, L., Weber, L., Helgerth, K. A. C., Stich, E., Günther, M., Tegethoff, P., Wagner, F. S., Ambrus, E., Antunovic, M., Barrera, F., Halali, E., Ioannidis, K., McKay, R., Milstein, N., Molden, D. C., Papenmeier, F., Rinn, R., Schreiter, M. L., Zimdahl, M., Allen, E., Bahník, S., Baumeister, R. F., Bermeitinger, C., Bickenbach, S. L. C., Blank, P. A., Blower, F. B. N., Bögler, H. L., Boo, F. L., Boruchowicz, C., Bühler, R. L., Burgmer, P., Cheek, N., N., Dohle, S., Dorsch, L., Dück, M. S., Fels, S.-A., Fischer, A. L., Frech, M.-L., Freira, L., Friedinger, K., Genschow, O., Harris, A., Hartig, B., Häusser, J. A., Hedgebeth, M., Henkel, M., Horvath, D., Hügel, J. C., Igna, E. L. E., Imhoff, R., Intelmann, P., Karg, A. H., Klamar, A., Klein, C., Klusmann, B., Knappe, E., Köppel, L.-M., Koßmann, L., Kraft, P., Kroworsch, M. K., Krueger, S. M., Kühling, S., Lagator, S., Lammers, J., Loschelder, D. D., Navajas, J., Norem, J., K., Novak, J. Onuki, Y., Page, E., Panse, F., Pavlovic, Z., Pearton, J., Rebholz, T. R., Rodgers, S., Röseler, J. J., Rostekova, A., Roßmaier, K. V., Sartorio, M., Scheelje, L., Schindler, S., Schreiner, N. B., Seida, C., Shanks, D. R., Siems, M.-C., Stitz, M., Starkulla, M., Stäglich, M., Thies, K., Thum, E., Undorf, M., Unger, B. D., Urlichich, D., Vadillo, M. A., Wackershauser-Sablotny, V., Wessel, I., Wolf, H., Zhou, A., & Schütz, A. (2022). OpAQ: Open Anchoring Quest, Version 1.1.48.95. https://dx.doi.org/10.17605/OSF.IO/YGNVB, Shiny app accessible [here](https://metaanalyses.shinyapps.io/OpAQ/)

The introductory sample is the CC-BY-4.0 mental-health pool from Dannheim et al.
(2025), [doi:10.5271/sjweh.4219](https://doi.org/10.5271/sjweh.4219).
Its 12 signed Hedges-g effects, CI-derived SEs, membership and changes are
attributed in `inst/examples/dannheim`. Existing Aksayli/Coles data retain their
verified source licences and attribution in `inst/COPYRIGHTS`.

## Related projects

- Allbritton et al. created [A General Tool for Living Meta-Analysis](https://dallbrit.shinyapps.io/Meta_regression_app/) to run and update meta-analyses online, which contains similar functionalities. Currently, it seems more feature-rich than metaUI, but more focused on meta-analysts rather than end users of insights - and less customisable. The app is accessible [here](https://dallbrit.shinyapps.io/Meta_regression_app/), more details are in the accompanying paper:

Allbritton, D., Gómez, P., Angele, B., Vasilev, M., & Perea, M. (2024). Breathing Life Into Meta-Analytic Methods. *Journal of Cognition*, 7(1).


Other related tools include [Metapsy](https://metapsy.org/) (analysis workflow
inspiration), the [Cooperation Databank](https://app.cooperationdatabank.org/)
(dataset exploration), [PsychOpen CAMA](https://cama.psychopen.eu/)
(cumulative meta-analysis), and the [taVNS HRV app](https://vinzentwolf.shinyapps.io/taVNSHRVmeta/)
(a Bayesian living meta-analysis). These are references, not endorsements or
sources of bundled code. See the [issue follow-up decisions](validation/issue-followup.md)
for the assessed feature overlap and remaining work.

## Headless correctness candidate

The development branch adds a versioned JSON build path while retaining editable
R/Shiny apps. See [reproduction and scientific contract](https://github.com/LukasWallrich/metaUI/blob/main/validation/reproduction.md),
[the tiny synthetic config](inst/examples/tiny.json), and its
[JSON schema](inst/examples/config.schema.json). Build and launch are separate:

First copy `tiny.json` and `tiny.csv` from `inst/examples` into a fresh, empty folder, then run:

```r
metaUI::build_app("tiny.json")
shiny::runApp("tiny-app")         # separate, blocking launch action
```

`generate_shiny()` also supports this workflow: supply `save_to_folder` and
`launch_app = FALSE`, then launch separately with `shiny::runApp("<folder>")`, passing the folder you
saved to (a bare `shiny::runApp()` runs the current working directory). With
`launch_app = TRUE` (the default when no folder is supplied), interactive printing
of the returned app launches Shiny and blocks the console until you stop the app.
Assigning the app object to a variable postpones launch until you print it.

Authors may name one primary model (`primary_model` in `generate_shiny()` or the
JSON config). Apps show it first and label the other rows as explorations; they never
substitute another model when it is unsupported.

Generation refuses a nonempty folder by default. p/N are optional for the primary
models; directional models require explicit direction. COR/ZCOR require explicit
variance scales. Read the generated validation report before interpreting results.
The introductory Dannheim data use signed Hedges g, not correlations. No public deployment is automatic.

[PsychOpen CAMA](https://leibniz-psychology.org/en/practices-and-tools-of-open-science/psychopen-cama)
is another platform for cumulative meta-analysis. metaUI's intended contribution is
an author-owned app with editable R source. See the bounded
[publication-readiness assessment](https://github.com/LukasWallrich/metaUI/blob/main/validation/publication-readiness.md).

### Empirical example and performance

Copy `inst/examples/dannheim` to a fresh working directory and run
`Rscript --vanilla build.R`. Launch separately with
`shiny::runApp("mental-health-app")`. The tiny CC-BY mental-health pool retains
12 exact benchmark member IDs and original signed Hedges g with CI-derived
native SE. The source-reference REML/Hartung–Knapp model is checked against
metafor; other models/diagnostics are explicitly exploratory. Read the example's
README/provenance before interpreting results. No benchmark corpus is deployed.

Generated apps reuse compatible fitted objects and retain three recent exact
fit selections per reader session; `cache_hit` discloses reuse. Custom models
default to no cache. Pass `options = list(fit_cache_entries = 0)` to `generate_shiny()` (or set
`"options": {"fit_cache_entries": 0}` in the build JSON) to disable caching;
keep it disabled for random models or code depending on external state. Uploads
are validated, stored in session state, and clear prior cached results.

[Performance measurements](https://github.com/LukasWallrich/metaUI/blob/main/validation/performance-optimisation.md) distinguish
faster p-curve calculations/fit reuse from cache hits and first-time fitting costs.
[Licence audit](https://github.com/LukasWallrich/metaUI/blob/main/validation/licensing-audit.md) records the licensed replacement of
the former Barroso sample. This development candidate is not a public release.

Downloads preserve the full current input dataset and saved filter selections; the
summary sheet describes the selected rows. An empty selection stays empty on
re-upload and reports no eligible rows, while the original inputs remain available.

### Practical equivalence and forest exports

Summary includes an interval assessment for a reader-chosen symmetric smallest
effect size of interest, on the displayed SMD or r scale. No bound is assumed.
The strict containment criterion uses the reported intervals without refitting;
model assumptions and small-sample limitations are shown beside the assessment.
Downloads record the bound and classification in an `equivalence` sheet.

Forest plots compare observed-effect intervals and available multilevel/RVE
summary intervals. PDF, PNG and CSV downloads use the same selected rows; the
CSV retains both fitting and displayed scales. The configured row limit also
applies to exports.

The [synthetic correlation example](inst/examples/correlations) demonstrates raw-r
variance conversion and checks the fitted summary against an independent metafor
reference.
