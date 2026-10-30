# Look up study metadata from DOIs at build time

Queries the free Crossref REST API once per distinct DOI and returns one
row per study for `generate_shiny(study_info = ...)`. Run this while
authoring: generated apps read the stored result and never contact
external services. DOIs that are missing, malformed, unknown to Crossref
or unreachable are reported in the `status` column and a message;
nothing is guessed or matched by title.

## Usage

``` r
fetch_study_metadata(
  x,
  study = "study",
  doi = "doi",
  mailto = Sys.getenv("METAUI_MAILTO"),
  abstracts = c("open_licence", "exclude_nc", "none", "all"),
  openalex = TRUE,
  openalex_key = Sys.getenv("OPENALEX_API_KEY"),
  cache = NULL,
  pause = 0.2
)
```

## Arguments

- x:

  A dataset from
  [`prepare_data()`](https://lukaswallrich.github.io/metaUI/reference/prepare_data.md)
  with a `url` mapping, or a data frame with the columns named by
  `study` and `doi`.

- study, doi:

  Column names used when `x` is not a prepared dataset.

- mailto:

  Contact email sent to Crossref, as its etiquette requests, to use the
  "polite" pool. Defaults to the `METAUI_MAILTO` environment variable;
  leave it unset to query anonymously.

- abstracts:

  `"open_licence"` (default), `"exclude_nc"`, `"none"`, or `"all"`; see
  Details.

- openalex:

  Use OpenAlex for licences and abstracts that Crossref lacks.

- openalex_key:

  Optional OpenAlex API key. Defaults to the `OPENALEX_API_KEY`
  environment variable.

- cache:

  Optional CSV path. Rows already retrieved under the same abstract
  policy are reused and only the remaining DOIs are queried; the result
  is written back. Review or edit it like any other input file.

- pause:

  Seconds between requests (both services allow several per second).

## Value

A data frame with one row per study: `study`, `doi`, `status` (`found`,
`no_doi`, `invalid_doi`, `not_found`, `conflicting_dois` or `error`),
`title`, `authors` (`"Family, Given; ..."`), `year`, `journal`,
`volume`, `issue`, `pages`, `abstract`, `abstract_licence` (a licence
URL from Crossref or a licence code from OpenAlex), `abstract_source`,
`abstract_note`, `metadata_source`, `abstract_policy` and `retrieved`.

## Details

Bibliographic fields are always kept. Abstracts are not covered by
Crossref's open metadata terms, so with the default
`abstracts = "open_licence"` an abstract is kept only when the article
is published under a Creative Commons licence (CC BY, with or without
SA, ND or NC terms), CC0 or the Public Domain Mark. The licence is taken
from the Crossref record (for the version of record, or an unspecified
version, that has started by the lookup date) or, failing that, from the
published-version locations that OpenAlex records. When Crossref has no
abstract, OpenAlex's abstract is used. OpenAlex is asked only when
Crossref leaves the abstract or its licence open.

NonCommercial (NC) licences permit use that is not primarily intended
for commercial advantage or monetary compensation, which covers academic
apps. If the app is used commercially, set `abstracts = "exclude_nc"` to
drop them. `abstracts = "all"` keeps every abstract found: the app
authors then take responsibility for the right to republish them, and
the app says so.

Single-work lookups by DOI are free on both services and need no key.
Crossref asks for a contact address: set `METAUI_MAILTO` (e.g. in
`~/.Renviron`). An OpenAlex key is optional and only raises the rate
limit; create a free account at <https://openalex.org>, copy the key
from Settings \> API key and set `OPENALEX_API_KEY`. The key is sent as
a request header and is not stored in the result or the app.

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- data.frame(study = c("Smith 2020", "Lee 2021"), d = c(.2, .4), vi = c(.02, .03),
                  doi = c("10.1371/journal.pone.0287911", "10.1037/0033-2909.112.1.155"))
app_data <- prepare_data(raw, "study", "d", variance = "vi", url = "doi")
info <- fetch_study_metadata(app_data, cache = "study-metadata.csv")
generate_shiny(app_data, "Example", study_info = info, save_to_folder = "app")
} # }
```
