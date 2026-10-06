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
  filter_popups = list(),
  save_to_folder = NA,
  launch_app = is.na(save_to_folder),
  ...,
  options = list(),
  overwrite = FALSE
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

- filter_popups:

  Named list with content for popup windows that provide further details
  on filter variables. They can contain HTML formatting, but should then
  be wrapped into `HTML()`, for instance:
  `list(Year = HTML("<i>Note:</i><br>This refers to data collection if reported, otherwise the publication year.`)

- save_to_folder:

  If specified, the code and data for the app will be saved to this
  folder. Defaults to NA, which means that nothing will be saved. If the
  folder exists, the user will refused unless overwrite = TRUE. Prefer a
  fresh destination to preserve manual edits.

- launch_app:

  Should the app be launched? Defaults to TRUE if it is not saved (i.e.
  save_to_folder is NA), FALSE otherwise.

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

## Value

If `launch_app = FALSE`, invisibly the normalised path of the generated
app folder. Otherwise a Shiny app object for the saved folder, which
launches when printed.

## Examples

``` r
# First, use prepare_data() to create your dataset.
if (exists("app_data")) {
  generate_shiny(app_data,
    dataset_name = "Your meta-analysis",
    eff_size_type_label = "Declared effect scale")
}
```
