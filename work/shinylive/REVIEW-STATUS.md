# PR 45 exploration review dispositions

Scope: only `work/shinylive/`. Base code: f1e546a; branch
`t3/fix-shinylive-review-findings`. No package-code changes or PR-description edits.

Historical records are retained as explained in README.md. “Fixed” below can
mean clarifying the evidence provenance, where changing historical numbers would
be inappropriate. No GitHub review replies or thread resolutions were posted.

| Comment ID | Disposition | Verified change / reason |
| --- | --- | --- |
| 4428612708 | Fixed | Bounded iframe polling with METAUI_STARTUP_TIMEOUT; missing-frame run exits 1 with a clear timeout. |
| 4428612715 | Fixed | Reproduction README links upstream webR issue 634. |
| 4428612726 | Fixed | Preserved skipped-test log; README distinguishes this run from the separately passed opt-in export test. |
| 4428612915 | Fixed | Preserved PASS 1242 record; README identifies it as historical. |
| 4428612737 | Fixed | Preserved intermediate failure log; fresh static and server smoke assert and pass link restoration. |
| 4428612756 | Fixed | Preserved historical JSON; README explains R stderr and favicon 404; fresh harness records console_info separately. |
| 4428612730 | Fixed | Removed forced #go click; static smoke waits for live handover and an enabled Analyze button, then clicks normally. |
| 4428612741 | Fixed | Attached no-op catches to all pending download-event promises, retaining awaited errors. |
| 4428612828 | Fixed | Invalid forest download content throws in browser and fallback paths; fallback failure propagates. |
| 4428612832 | Fixed | Link restore requires restored notice, Selected 11 of 12, and Source-reference results; failures propagate. |
| 4428613032 | Fixed | Removed unused BAYES option and usage hint. |
| 4428612749 | Fixed | Regenerated manifest from fresh current-code Dannheim export; patched packages have source URLs. |
| 4428612871 | Fixed | Regenerated STATIC-NOTICE.md and bundled-packages.csv from fresh export; generation date documented. |
| 4428612763 | Fixed | Removed private preview URLs and home-directory paths; full folder text scan is clean. |
| 4428612775 | Fixed | CSV verified: 3 differences / 29 direct; 14 / 127 recursive. Report states both. |
| 4428612959 | Fixed | Report states supplied validated 98 tests / 1,620 expectations at f1e546a and separate opt-in pass. |
| 4428612779 | Fixed | Missing ready/results grep matches no longer abort under pipefail; null remains available. |
| 4428612889 | Fixed | Robust int(code) comparison handles HTTPStatus and invalid/default codes. |
| 4428612896 | Fixed | RUN_INDEX offsets loop numbering; existing JSONL receives chronological pair fields without numeric changes. |
| 4428612836 | Fixed | Independent verifier explicitly whitelists documented convergence categories and exact datasets; other errors/differences fail. |
| 4428612855 | Fixed | Imputation sum uses na.rm=TRUE; prints categorized errors and counts. See validation note for paired REML convergence failures. |
| 4428612850 | Not changed (already implemented) | Already labels rows by Model. No implementation change needed for labels; preserved historical text output. |
| 4428612924 | Fixed | Asserts exactly one Source-reference row before printing output. |
| 4428612944 | Fixed | Documents required libraries/scratch directories; research scripts accept output paths by arguments or environment with defaults. |
| 4428612976 | Fixed | Header names metafor-helpers-equivalence.R correctly. |
| 4428639660 | Fixed | Unexpected page/console errors now fail smoke after narrow classification; errors include source URLs. |
| 4428639669 | Fixed | Idle timestamp resets before returning true, so each call requires a fresh stable interval. |
| 4428639663 | Fixed | Preserved Playwright run; evidence index explains automation overhead versus real-timing.sh. |
| 4428639688 | Fixed | Preserved old meta-preload timings; README labels pre-migration configuration and harness. |
| 4428639738 | Clarified; time-origin concern not reproduced | Harness at 5b45113 confirms first_analysis_s is click-to-results, including the lazy experiment. README documents configuration, provenance and superseded status; exact experimental export was not retained. |
| 4428639771 | Fixed | README defines METAUI_T/METAUI_P and preload passes; notes exact temporary instrumentation command was not retained. |
| 4428639746 | Fixed | Historical JSON preserved; README explains R stderr/favicon classification and superseded status. |
| 4428639751 | Fixed | README explicitly identifies run-static.json as predating the successful link-restore fix. |
| 4428639707 | Fixed | Preserved dated parity text; README clarifies manual summary and excludes two unsupported NA model rows (six numeric rows). |
| 4428639766 | Fixed | Fixed ordinary-group filtering for quantity names; regenerated summary from retained comparisons, removing only two spurious unavailable rows. |
| 4428639715 | Fixed | timed-run.sh requires caller browser/module paths with clear environment errors; no home defaults. |
| 4428639758 | Fixed | real-timing.sh requires METAUI_CHROMIUM; no home defaults. |
| 4428639725 | Fixed | Directory URL logging resolves index.html before file-size check; checked actual served size. |
| 4428639728 | Fixed | OUT is checked before requiring Playwright or launching a browser. |
| 4428639775 | Fixed | pmax uses na.rm=TRUE. |
| 4428639781 | Fixed | Notice copied verbatim from fresh export, including source-archive sentence. |
| 4428686412 | Fixed | Replaced hard expectation count with the rendering/selection-update behavior checked. |
| 4428686430 | Fixed | Research README refers to the ordinary-summary CSV as the numeric source of truth. |
| 4428686438 | Fixed | Smoke absolute keys use _s and match startup iframe_s/ready_s; navigation origin and operation durations documented, including historical exceptions. |
| 4428686458 | Fixed | Workbook comparison enforces configurable tolerance, matching NA patterns, model/status/warning equality, and finite numeric pairs. |

Validation: fresh export, static/server browser smoke (including strict link
restore), real-timing.sh, independent 400-dataset verifier, workbook parity and
negative workbook fixtures. Historical evidence remains unchanged except the
explicitly requested pair labels, regenerated export artefacts and corrected
ordinary-summary grouping. New review captures are separate files. The independent
run passed with maximum differences 1.78e-15 (trimfill), 3.55e-15 (REML/HK), zero
k0 mismatches and exactly the documented convergence cases.
