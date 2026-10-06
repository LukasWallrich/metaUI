# metaUI 0.2.0 release preparation

The release candidate includes the correctness, UX, issue-followup and completed
analysis features in the stacked PRs. Version and NEWS are updated; examples build
into fresh temporary directories without starting a server. bayesmeta is optional
(Suggests); enabled generated apps check it and record its installed version.
Runtime imports remain required by generated standalone apps.

The included-code/data licensing review is in `validation/licensing-audit.md`
and `inst/COPYRIGHTS`. No deprecated API transition requires advancement.
The source was fetched before stacking; changes are pushed through the PR stack.

Local evidence is in `/home/lukas/work/metaui-complete`: source and installed test
logs, independent Bayesian quadrature test, numerical benchmarks, generated-app
browser checks, build/check logs, URL and spelling checks. Vignettes use only
bundled data and build offline. The manual uses the existing local TinyTeX.

URL checking corrected redirected package/documentation links. Public Shiny apps
occasionally time out at urlchecker's five-second limit; they are references and
were rechecked with a 30-second curl timeout; availability is recorded in the check logs. Spelling output is reviewed for real
errors; most flagged words are proper names, package/API identifiers and British
spellings in references and documentation.

## Completed checks

* Independent posterior quadrature and computation/URL/configuration tests pass.
* Full source suite passed functional tests; a documentation-helper parity mismatch
  was corrected and verified. Installed as-CRAN suite passes (one existing
  source-tree-only parity skip).
* R CMD build compiles both bundled vignettes offline. As-CRAN check with local
  TinyTeX and HTML Tidy passes code, examples, tests, vignettes and both manuals.
  One incoming NOTE records original p-curve URLs returning HTTP 406 to the checker;
  no example downloads these. `cran-comments.md` explains the retained attribution
  and generated-app runtime Imports. DOI references use the Rd macro.
* Real browser feature checks verify Bayesian labels/sensitivity, the computation
  selector/comparisons, link restoration without a bound, wrong-build refusal,
  workbook upload restoration, clearing the address bar for uploads, and downloads.
  The downloaded workbook contains primary/alternative inputs, applied summary,
  computation comparison, sensitivity and provenance, checked independently in R.
* The original generated-app browser smoke checks cover numeric/picker/NA restoration,
  forest exports, sample tables and reset. Hosted Linux release/oldrel, macOS and
  Windows checks passed on the initial commit; the browser regression in source-p
  formatting was fixed and both local browser suites now pass. The final hosted suite reruns.

Public CRAN submission, win-builder/rhub services that send email, release tags,
GitHub publication and public announcements require the authors' final approval.
No submission, email or public release is performed by this preparation.

## Announcement draft

metaUI 0.2.0 makes interactive meta-analyses easier to inspect and share. Authors
can build a standalone Shiny app from R or declarative JSON, with explicit effect
and variance scales, study/effect identities, dependency versions and validation
provenance. Readers can compare author-defined effect-size computations, assess
practical equivalence using their own bound, export forest plots, and share an
applied selection through a validated link. Optional Bayesian study-level models
report posterior medians, central credible intervals and prior sensitivity.

The release also corrects dependence handling and statistical diagnostics,
strengthens edited-workbook upload/download consistency, improves keyboard and
mobile access, and adds independent numerical references and browser regression
checks. The documentation explains model assumptions and where estimators are
unsupported. These analyses remain conditional on the authors' scientific choices.

Private review app: http://100.121.34.85:7895/ (synthetic data only).
The server remains available on the tailnet; no public deployment is made.
