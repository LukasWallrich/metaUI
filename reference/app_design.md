# Choose a generated app's design

Create settings for
[`generate_shiny()`](https://lukaswallrich.github.io/metaUI/reference/generate_shiny.md)
or
[`design_app()`](https://lukaswallrich.github.io/metaUI/reference/design_app.md).
Colours must be opaque six-digit hex values. Text and accent colours
must have at least 4.5:1 contrast against the page, panels and derived
tints. The default font is the open-licensed Source Sans and Source
Serif pair, which ships inside each app. The other fonts come from each
reader's device, use fallbacks and may be substituted on other
platforms.

## Usage

``` r
app_design(
  accent = "#1D5C48",
  background = "#FFFFFF",
  surface = "#FFFFFF",
  text = "#1A2530",
  font = "source",
  text_size = 16,
  spacing = "comfortable",
  radius = 3
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

  One of `source`, `system`, `arial`, `verdana` or `georgia`.

- text_size:

  Base interface text size in pixels, from 14 to 20.

- spacing:

  `comfortable` or `compact`.

- radius:

  Corner radius of buttons and fields in pixels, from 0 to 16.

## Value

A named list of validated design settings.

## Examples

``` r
design <- app_design(accent = "#69429A", font = "georgia", radius = 4)
# generate_shiny(dataset, "My analysis", design = design)
```
