# Getting started

This vignette shows how to use `metaUI` to generate a Shiny app in a few
simple steps. Firstly, you need to make sure that you have the package
installed, and then load it.

``` r

# Paste the following two lines to your Console and remove the # to install the package
# if (!require(remotes)) install.packages("remotes")
# remotes::install_github("LukasWallrich/metaUI")

library(metaUI)
library(dplyr)
```

## Prepare the data

In a first step, you will need to load and import data. For metaUI, your
datafile needs to contain:

- a **study label** to identify individual samples (You can include
  multiple effect sizes with the same study label to indicate that they
  are dependent.),
- an **effect size** measure,
- the **variance** or the **standard error** of the effect size (at
  least one; the other is derived)
- optionally, the **sample size** it is based on (needed by p-curve, not
  the primary fits)
- optionally, source **p-values**, retained with their provenance
- data on any **moderators / filters** you want to include

Note that some of the fields can be calculated based on others. If you
have effect sizes and their variance, for instance, you can calculate
the standard error (by taking the square root of the variance), or if
you have effect sizes, sample sizes and *p* values, you can calculate
their standard error using the `se.from.p` function included in this
package.

As a first step towards your metaUI app, **load the data.**

``` r

app_data <- read.csv(system.file("examples", "dannheim", "mental-health.csv", package = "metaUI"))
```

These 12 mental-health effects come from Dannheim et al. (2025),
[doi:10.5271/sjweh.4219](https://doi.org/10.5271/sjweh.4219), licensed
CC-BY-4.0. Each row is the authors’ composite for a distinct study. The
original signed Hedges g is retained; negative means improvement. The
normal 95% CI-width rule provides `se_g`, and variance is its square.
Rounded inputs reproduce the published REML/Hartung–Knapp result
approximately. Read the packaged example’s README and provenance before
interpretation; no sample counts or p-values are inferred. The default
metaUI model rows are explorations with different methods; copy the
example’s `models.R` to include its verified source-reference row.

Map the effect, uncertainty and study/effect identifiers explicitly.
Direction is unspecified because a negative improvement convention does
not itself declare a one-sided selection-model hypothesis. No moderator
coding is invented here. For your own data, map documented
numeric/factor moderators via `filters`.

``` r

app_data <- prepare_data(app_data,
  study_label = "study", es_label = "study", es_id = "row_uid",
  es_field = "g", se = "se_g", es_type = "SMD", variance_scale = "SMD",
  direction = "unspecified")
```

Correlation datasets need an explicit variance-scale contract: `COR`
uses raw r and raw-r variances, then transforms once; `ZCOR` uses Fisher
z and z-scale variances directly. Summary estimates back-transform to r,
while diagnostics retain the fitting scale. If a normal Wald
approximation is scientifically appropriate, its two-sided p-value is
`2 * pnorm(-abs(effect / se))`; this is an approximation with declared
assumptions, not a recovered source study p-value.

## Run the app

Now you are ready to launch a first version of the app. This is useful
to see whether the data is read correctly, and to identify what
customization you want to make … or if you just want to explore your
dataset locally. After you run the line below, the app should open your
web browser.

``` r

generate_shiny(app_data,
               dataset_name = "Dannheim et al. (2025) - Mental health",
               eff_size_type_label = "Hedges g (negative = improvement)",
               options = list(shiny_theme = "sandstone", selection_list_threshold = 3))
```

## Save the app

To then create the Shiny app in a format that can be customized and
deployed to shinyapps.io, you need to provide some more details
regarding the information that should be displayed and **the path that
the app should be saved to**.

``` r

generate_shiny(app_data,
               dataset_name = "Dannheim et al. (2025) - Mental health",
               eff_size_type_label = "Hedges g (negative = improvement)",
               save_to_folder = "my_app_folder",
               citation = "Dannheim et al. (2025). doi:10.5271/sjweh.4219. CC-BY-4.0. Reanalysis, not author endorsement.")
# add your own contact address via `contact = "..."` if you want readers to reach you
```

If one model is your primary analysis, name it with `primary_model`,
using the model’s `name` exactly as it appears in the models tibble or
`models.R` (for example
`primary_model = "Random-Effects Multilevel Model"`). The app shows that
model first and labels the other rows as explorations. If it cannot be
estimated for a reader’s selection, the app says why and does not
substitute another model. Without `primary_model`, the app states that
it does not choose between estimators. The declaration is stored in the
generated `labels_and_options.R` and in `validation.json`.

When you run this, the app will be saved to a new folder, in this case
`my_app_folder`. You can run `shiny::shinyAppDir(my_app_folder)`
(specifying your folder) to run the Shiny app.

To further customize the app, consider editing:

- `models.R` to adjust the meta-analytic models that are compared on the
  main page. See vignette(“customise_models”) for instructions on how to
  do that.

- `ui.R` to edit the user interface (e.g., labels or the About section)
  or to drop certain parts of the output apart from the main models
  (e.g., to display only a *z*-curve or only a *p*-curve),

- `server.R` to modify other other parts of the output (for instance, to
  use a different theme for the graphs),

- `labels_and_options.R` to change the dynamically displayed text (e.g.,
  the welcome message) and some fundamental options (e.g., for
  aggregating dependent effect sizes).

## Deploy to shinyapps.io

If you want to share the app with the wider world, shinyapps.io is a
good platform, with a free plan that is sufficient to start with. If you
have not used it before, follow the instructions provided by
Posit/RStudio to get started
[here](https://shiny.rstudio.com/articles/shinyapps.html). In essence,
you need to create an account, install the `rsconnect` package and
authenticate within that package. Then you can run the following to
deploy your app:

``` r

rsconnect::deployApp(appDir = "my_app_folder")
```

Once that is done, the app should open in your browser, and you can copy
the link from the address bar. That’s it - you now have your own
meta-analysis Shiny app online.
