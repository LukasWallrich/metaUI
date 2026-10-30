# Static (WebAssembly) metaUI app

This folder runs a metaUI app entirely in the reader's browser. Serve the export
folder that contains `shinylive-sw.js` over HTTPS or from localhost; for example, run
`python3 -m http.server` there and open http://localhost:8000/ (plus this app's
subfolder, if any). Opening index.html as a file does not work.

**Data:** the full dataset is downloaded by every visitor. Publish only data that may be shared.

## Bundled third-party software

- shinylive web assets (Posit, PBC; MIT licence): https://github.com/posit-dev/shinylive
- webR, a WebAssembly build of R (webR binaries: GNU GPL version 3; R: GNU GPL 2 or 3):
  https://github.com/r-wasm/webr
- 128 R packages built for WebAssembly by https://repo.r-wasm.org, listed with versions and licences in `bundled-packages.csv`. Source code for CRAN versions is available from CRAN (current and archived releases) at the listed URL; webR-patched builds are marked as such.

The About tab lists package versions on the authoring computer. The versions that
run in the browser can differ and are listed in `bundled-packages.csv`.

The app code itself (ui.R, server.R, and the other R files) is part of this export and
carries the licence stated in its COPYRIGHTS file.
