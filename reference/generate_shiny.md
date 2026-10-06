# Generate Shiny app

This function generates the Shiny app. It is the main function of the
package. The app can either be launched directly, or saved to a folder.
If saved, that folder will contain all the code and data needed to run
the app. The app can then be launched using
[`shiny::shinyAppDir()`](https://rdrr.io/pkg/shiny/man/shinyApp.html) on
that folder, or modified by editing server.R, ui.R, global.R and
models.R in the folder.

## Usage

``` r
generate_shiny(
  dataset,
  dataset_name,
  eff_size_type_label = NA,
  models = get_model_tibble,
  primary_model = NULL,
  filter_popups = list(),
  save_to_folder = NA,
  launch_app = is.na(save_to_folder),
  ...,
  options = list(),
  overwrite = FALSE,
  bayesian = NULL
)
```

## Arguments

- dataset:

  Dataframe as returned from
  [`prepare_data()`](https://lukaswallrich.github.io/metaUI/reference/prepare_data.md)

- dataset_name:

  Name of the dataset

- eff_size_type_label:

  The label to be used to describe the effect size. If NA, effect type
  code from dataset is used.

- models:

  The models to be included in the app. Can either be a function to
  call, a tibble, or the path to a file. If you want to change the
  default models, have a look at the vignette and/or the
  [`get_model_tibble()`](https://lukaswallrich.github.io/metaUI/reference/get_model_tibble.md)
  documentation. Passing a file (e.g. "my_models.R") is particularly
  helpful if you include helper functions and save the app. If so, this
  must assign the tibble to a variable called models_to_run (i.e. using
  \<-).

- primary_model:

  Optional name of the one model (a `name` in `models`) that the authors
  treat as their primary analysis. Generated apps show it first and
  label the other rows as explorations. If it cannot be estimated for a
  reader's selection, the app reports why and does not substitute
  another model. The default, `NULL`, declares no primary model, and the
  app then says that it does not choose between estimators.

- filter_popups:

  Named list of expandable filter-help content. Plain text is escaped;
  wrap trusted HTML in
  [`htmltools::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html),
  for example
  `list(Year = htmltools::HTML("<i>Note:</i> Data collection year."))`.

- save_to_folder:

  Folder for the generated code and data. Defaults to NA, which uses a
  temporary folder. A nonempty destination is refused unless overwrite =
  TRUE. Prefer a fresh destination to preserve manual edits.

- launch_app:

  Should the app be launched? Defaults to TRUE if save_to_folder is NA,
  FALSE otherwise. Interactive auto-printing of the returned app
  launches it and blocks the R console until the app stops. Use
  launch_app = FALSE to build without launching, then run
  shiny::runApp() separately when ready.

- ...:

  Arguments passed on to
  [`create_about`](https://lukaswallrich.github.io/metaUI/reference/create_about.md)

  `date`

  :   Date of the last update. Defaults to the current date.

  `citation`

  :   Citation requested by the author. Defaults to an empty string.

  `osf_link`

  :   Link to the OSF (or similar) project where data and materials are
      available. Defaults to an empty string.

  `contact`

  :   Contact information, typically email. Defaults to an empty string.

  `list_packages`

  :   Should all packages used by the app be listed on the About page?
      If TRUE, it list the packages used in the default configuration.
      To display different packages (e.g., because you added/removed
      models), pass a character vector with all packages to display.

- options:

  List of more detailed options to customise your app. They all have
  sensible defaults and are thus rarely needed.

  - `max_forest_plot_rows` Numeric. What is the maximum number of
    effects for which a forest plot should be displayed? Defaults
    to 200. If more effect sizes are selected, a message is shown
    instead.

  - `shiny_theme` Character. One of the shinythemes that style the app.
    Defaults to "yeti", see
    [`?shinythemes::shinythemes`](https://rdrr.io/pkg/shinythemes/man/shinythemes.html)
    for all options.

  - `fit_cache_entries` Integer 0 to 3. Recent exact selections per
    reader session; 0 disables caching. Defaults to 3 for default models
    and 0 for custom code.

  - `selection_list_threshold` Numeric. From how many filter levels
    should a selection box be shown instead of check boxes? Defaults to
    6.

- overwrite:

  Explicit opt-in to replace generated files in a nonempty destination.
  Unrelated files are retained.

- bayesian:

  NULL (default), or list(enabled = TRUE, tau_scale = .5, max_studies =
  20). Adds a deterministic study-level normal-normal model with a flat
  prior on the mean and a half-normal heterogeneity prior. Correlation
  default tau_scale is .25 on Fisher z. Requires optional bayesmeta.
  Sensitivity analyses use half and double the scale.

## Value

With launch_app = FALSE, invisibly returns the generated app folder
path. With launch_app = TRUE, returns a Shiny app object; printing it
launches the app and blocks the console until it stops.

## Examples

``` r
raw <- data.frame(study = letters[1:6], d = c(.1, .3, -.1, .4, .2, .5),
                  vi = c(.02, .03, .02, .04, .01, .05))
app_data <- prepare_data(raw, "study", "d", variance = "vi")
folder <- tempfile("metaui-example-")
generate_shiny(app_data, dataset_name = "Example", save_to_folder = folder,
               launch_app = FALSE)
unlink(folder, recursive = TRUE)
```
