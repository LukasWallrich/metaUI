# Import data from a file and return a data frame

You may want to use the
[`metafor::escalc()`](https://wviechtb.github.io/metafor/reference/escalc.html)
function in the metafor package to calculate effect sizes and their
variances in preparation for this function.

## Usage

``` r
prepare_data(
  data,
  study_label,
  es_field,
  se = NULL,
  pvalue = NULL,
  sample_size = NULL,
  variance = NULL,
  filters = character(),
  url = NA,
  es_type = "SMD",
  article_label = NA,
  es_label = NA,
  na.rm = "es_related",
  arrange_filters = c("given", "alphabetical", "leave"),
  keep_missing_level = FALSE,
  es_id = NULL,
  variance_scale = NULL,
  direction = c("unspecified", "positive", "negative"),
  categorical_filters = character()
)
```

## Arguments

- data:

  path to the .csv file to read OR a data frame

- study_label:

  Character. Name of the field to use as the id/study label

- es_field:

  Character. Name of the field to use as the effect size

- se:

  Character. Name of the field to use as the standard error for the
  effect size.

- pvalue:

  Character. Name of the field to use as the p-value for the effect
  size.

- sample_size:

  Character. Name of the field to use as the sample size

- variance:

  Character. Name of the field to use as the sampling variances.

- filters:

  Character. List of fields to use as filters - can be named if
  different labels should be displayed

- url:

  Character. Field with URLs or DOIs to link to. DOIs can be in the
  format "10.1234/5678" or full links. Defaults to NA.

- es_type:

  Character. Input scale: SMD, COR (raw r), or ZCOR (Fisher z). COR
  needs r-scale variance and uses delta-method variance conversion; ZCOR
  needs z-scale variance.

- article_label:

  Character. Field with article labels. Only used to report number of
  references in addition to number of independent samples.

- es_label:

  Character. Label for individual effect sizes - only needed when there
  are multiple effect sizes per study/sample. Defaults to NA. Defaults
  to NA, in that case, multiple effect sizes are simply numbered.

- na.rm:

  Should rows with any missing values be removed? Can be TRUE, FALSE or
  "es_related" - the last is the default and drops rows with missing
  values for any of the variables used in the standard meta-analysis
  models, namely `study_label`, `es_field`, `variance`, and `se`.
  Optional p/N never remove rows under this default. Setting this to
  TRUE also drops rows with missing values on any of the filters etc,
  which might often be unnecessary. Conversely, setting this to FALSE
  might lead to issues in the model - unless you post-process the data
  or change the models and analyses to be included in the app. Invalid
  required inputs with FALSE cause an error.

- arrange_filters:

  Character. How should the filters be arranged in the app? Options are
  "given" by the `filters` argument, "alphabetical" or "leave" (as they
  are in the dataset). Defaults to "given".

- keep_missing_level:

  Logical. Should a `(Missing)`-level be kept even for filters that do
  not have missing values? Might be advisable when you expect users to
  upload new data with missing values. Defaults to FALSE.

- es_id:

  Optional column with unique effect IDs within each study. Defaults to
  input row IDs.

- variance_scale:

  Required for COR (r) and ZCOR (z); optional SMD for SMD.

- direction:

  Explicit direction for one-sided bias models: unspecified, positive,
  or negative.

- categorical_filters:

  Explicitly convert named filter columns to factors with radix-sorted
  levels. Useful for CSV/JSON builds.

## Value

tibble with the data from the file/input reformatted for metaUI

## Examples

``` r
if (FALSE) { # \dontrun{
prepare_data("my_meta.csv", "study_id", "cohens_d", variance = "vi")
} # }
```
