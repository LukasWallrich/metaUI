# Preview and export an app design

A local Shiny helper with colour, font and layout controls. The preview
uses the same stylesheet and design settings as generated apps, with
illustrative results. Copy or download the R settings and pass them to
[`generate_shiny()`](https://lukaswallrich.github.io/metaUI/reference/generate_shiny.md).
The JSON download is the object used by
[`build_app()`](https://lukaswallrich.github.io/metaUI/reference/build_app.md)'s
`design` field.

## Usage

``` r
design_app(design = app_design())
```

## Arguments

- design:

  Initial settings from
  [`app_design()`](https://lukaswallrich.github.io/metaUI/reference/app_design.md)
  or a named list.

## Value

A Shiny app object. Printing it launches the helper; assigning it does
not.

## Examples

``` r
if (interactive()) design_app()
```
