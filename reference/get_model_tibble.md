# Get model specifications

This function contains the model specifications that will be compared in
the Shiny app. If you want to maintain the default, just call
[`generate_shiny()`](https://lukaswallrich.github.io/metaUI/reference/generate_shiny.md)
without specifying the models argument. If you want to change the
default models, have a look at the vignette and/or the source code of
this function (by running `get_model_tibble` in the console) and modify
it accordingly.

## Usage

``` r
get_model_tibble()
```

## Value

A tibble with the models to be included in the app - including details
on how to estimate them and how to extract the relevant information from
the model output.

## Examples

``` r
# Run the function to get the models
mod <- get_model_tibble()
# Modify them as desired
mod$name[1] <- "RE 2-level model"
# Then use when generating the app
if (exists("app_data")) {
  generate_shiny(app_data,
    dataset_name = "Your meta-analysis",
    eff_size_type_label = "Declared effect scale")
}
```
