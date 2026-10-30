# Silent startup error in isolated renderPlot dimensions never invalidates in webR

Environment: shinylive assets 0.10.12, webR 0.6.0, bundled Shiny 1.13.0.8000.
Native Shiny 1.14.0 recovers correctly. No upstream issue has been filed.

Export this directory with `shinylive::export()` and serve it over localhost.
Click Analyze: the comparison stays blank with a `shiny.silent.error` / validation
error. The same app on native Shiny displays a plot and updates on each click.
No spinner, other packages, or metaUI code is needed. This minimal app was
verified in Chromium: after Analyze, the error is silent validation, the output
value is undefined, and the image count is zero.

The bundled `renderPlot()` is identical to native Shiny, but `hybrid_chain()` is
patched. Native Shiny uses an exiting `tryCatch()` error handler. webR uses
`.Internal(.addCondHands(..., TRUE))`, a calling handler, plus a delayed `do_catch`.
`renderPlot()` initially calls `isolate(getDims())`. The reactive height calls an
`eventReactive()` before Analyze and raises a silent validation error. The
`drawReactive` catch handler deliberately calls `getDims()` again to establish
reactive dependencies. In native Shiny the isolated context has already unwound;
in the webR patch the calling handler still runs within the nested
`eventReactive` context, before that context and its enclosing isolation unwind.
The second dimension read consequently fails to establish the dependency that
would invalidate the plot after Analyze. The cached error persists.

Evidence: replacing only native Shiny's `hybrid_chain()` with the extracted webR
implementation reproduces the failure in `shiny::testServer()`, without a browser.
The fixture is `tests/testthat/fixtures/webr-hybrid-chain.R`; the regression test
exercises the generated server before Analyze and after two different selections.
A context probe records `getDims()` first in `[isolate]`, then in `plotObj`
with native Shiny; with the patched helper its second call instead runs in
`eventReactive({ input$go })`.
Before the fix the regression test fails after Analyze with the same silent error; after the fix
all six expectations pass. The static smoke test also fails on the original app
at its explicit `#model_comparison img` natural-width check.

Workarounds verified in the minimal native reproduction with the webR helper:
`execOnResize = TRUE`, or avoiding a silent error in the initial height function.
metaUI uses the latter: return its existing minimum height (103 px) until
`input$go` is truthy. This leaves its plot expression, post-analysis dimensions,
and normal resize replay behavior unchanged.

For upstream investigation, preserve exiting-handler semantics when implementing
`hybrid_chain()` error recovery, or explicitly restore the reactive context before
executing its catch callback. A general patch belongs upstream; metaUI only guards
its own initial dimensions.
