# Choose a generated app's design

Create settings for
[`generate_shiny()`](https://lukaswallrich.github.io/metaUI/reference/generate_shiny.md)
or
[`design_app()`](https://lukaswallrich.github.io/metaUI/reference/design_app.md).
Colours must be opaque six-digit hex values. Text and accent colours
must have at least 4.5:1 contrast against the page, panels and derived
tints. Fonts come from each reader's device and use fallbacks; no
author-machine font files are needed. Named fonts may be substituted on
other platforms.

## Usage

``` r
app_design(
  accent = "#096B74",
  background = "#EDF3F7",
  surface = "#FFFFFF",
  text = "#182F46",
  font = "system",
  text_size = 15,
  spacing = "comfortable",
  radius = 10
)
```

## Arguments

- accent:

  Colour for actions, links and primary estimates.

- background:

  Page background colour.

- surface:

  Panel background colour.

- text:

  Main text colour.

- font:

  One of `system`, `arial`, `verdana` or `georgia`.

- text_size:

  Base interface text size in pixels, from 14 to 20.

- spacing:

  `comfortable` or `compact`.

- radius:

  Panel corner radius in pixels, from 0 to 16.

## Value

A named list of validated design settings.

## Examples

``` r
design <- app_design(accent = "#69429A", font = "georgia", radius = 4)
# generate_shiny(dataset, "My analysis", design = design)
```
