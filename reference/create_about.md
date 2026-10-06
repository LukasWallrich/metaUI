# Create the about text for the app

This function is generally only called internally by
[`generate_shiny()`](https://lukaswallrich.github.io/metaUI/reference/generate_shiny.md).

## Usage

``` r
create_about(
  dataset_name,
  date = format(Sys.Date(), "%d %b %Y"),
  citation = "",
  osf_link = "",
  contact = "",
  list_packages = TRUE
)
```

## Arguments

- dataset_name:

  Name of the dataset

- date:

  Date of the last update. Defaults to the current date.

- citation:

  Citation requested by the author. Defaults to an empty string.

- osf_link:

  Link to the OSF (or similar) project where data and materials are
  available. Defaults to an empty string.

- contact:

  Contact information, typically email. Defaults to an empty string.

- list_packages:

  Should all packages used by the app be listed on the About page? If
  TRUE, it list the packages used in the default configuration. To
  display different packages (e.g., because you added/removed models),
  pass a character vector with all packages to display.

## Value

HTML code for the `About` panel of the Shiny app
