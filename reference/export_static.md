# Export a generated app as a static website (experimental)

Converts an app folder written by
[`generate_shiny()`](https://lukaswallrich.github.io/metaUI/reference/generate_shiny.md)
or
[`build_app()`](https://lukaswallrich.github.io/metaUI/reference/build_app.md)
into static files that run R in the reader's browser through 'shinylive'
(WebAssembly R). The result can be served by any static web host that
supports HTTPS, such as GitHub Pages, or opened from `localhost`; no
Shiny server is needed.

## Usage

``` r
export_static(
  app_dir,
  dest_dir,
  subdir = "",
  overwrite = FALSE,
  precompute = TRUE,
  include_sources = FALSE
)
```

## Arguments

- app_dir:

  Folder of a generated metaUI app.

- dest_dir:

  Folder for the static site. Must not exist unless `overwrite = TRUE`.

- subdir:

  Optional subfolder of `dest_dir` for this app, so that several apps
  can share one set of WebAssembly assets.

- overwrite:

  Replace an existing app in `dest_dir`/`subdir`? A nonempty target must
  contain `shinylive-sw.js`, or both `app.json` and `index.html`,
  identifying a previous export. Otherwise use an empty folder.
  Replacement deletes the entire target, including hand edits and
  unrelated files. With `subdir`, only that app subfolder is deleted;
  without it, all of `dest_dir` is deleted and rewritten. Keep hand
  edits in the editable app folder.

- precompute:

  Show precomputed default Summary results while the interactive app
  loads? Failures warn and export the interactive app alone. No browser
  is required.

- include_sources:

  Bundle exact copyleft package, R and webR source archives in
  `dest_dir/sources/`? Recommended for archival deposits such as Zenodo.
  Live sites can use the source links in `bundled-packages.csv` instead.

## Value

The path of the exported app, invisibly.

## Details

Use `generate_shiny(static_folder = ...)` for one-step generation and
static export. After editing the generated app files, call
`export_static()` to re-export the edited app; regenerating would
overwrite those edits.

The app folder is left unchanged. The export appends a small script to
the copied `www/metaui.js` so that selection links and downloads work
inside the shinylive frame, bundles WebAssembly builds of all required R
packages, and writes `STATIC-NOTICE.md` and `bundled-packages.csv` with
the versions and licences of the bundled software.

By default, the generated server computes the initial Summary at export
time. Readers see these results with disabled controls while the
interactive app loads. Selection links skip the preview. Export
provenance is stored in `metaui-preview/provenance.json`. Readers
download about 140 MB; models fit more slowly than on a server. The
dataset is shipped to every reader, so only export data that may be
published.

When apps share `dest_dir`, their package inventories, notices and About
tables describe the shared runtime: they can include packages used only
by another app. They are inventories of available software, not proof
that each app uses every package. `include_sources` collects the union
of copyleft packages in the shared runtime, preserving earlier source
archives.

## Examples

``` r
if (FALSE) { # \dontrun{
export_static("my-app", "my-app-static")
# Preview locally (service workers need https or localhost):
httpuv::runStaticServer("my-app-static")
} # }
```
