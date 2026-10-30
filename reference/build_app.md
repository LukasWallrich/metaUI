# Build an app from a declarative JSON configuration

Relative data and output paths resolve against the configuration file.
Building never launches or deploys an app. The configuration is
deliberately limited to data mapping, scientific scale/direction, an
optional declared primary model, metadata, presentation options, and
optional study details (schema version 2). `study_info` is either the
path to a CSV of study details or a lookup object,
`{"cache": "study-metadata.csv", "abstracts": "open_licence", "openalex": true}`,
which runs
[`fetch_study_metadata()`](https://lukaswallrich.github.io/metaUI/reference/fetch_study_metadata.md)
on the mapped `url` column and writes its result to the cache CSV. Only
the cache is required. The lookup is the only network access during a
build; DOIs already found in the cache are not queried again. The
generated app never queries external services.

## Usage

``` r
build_app(config, overwrite = FALSE)
```

## Arguments

- config:

  Path to JSON, or a named list with the same schema.

- overwrite:

  Replace generated files in a nonempty output folder, as in
  [`generate_shiny()`](https://lukaswallrich.github.io/metaUI/reference/generate_shiny.md).
  Unrelated files are retained. Checked before any lookup.

## Value

Invisibly, the generated app directory.
