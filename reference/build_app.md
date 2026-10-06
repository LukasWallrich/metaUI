# Build an app from a declarative JSON configuration

Relative data and output paths resolve against the configuration file.
Building never launches or deploys an app. The configuration is
deliberately limited to data mapping, scientific scale/direction, an
optional declared primary model, metadata, and presentation options.

## Usage

``` r
build_app(config)
```

## Arguments

- config:

  Path to JSON, or a named list with the same schema.

## Value

Invisibly, the generated app directory.
