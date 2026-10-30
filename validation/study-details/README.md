# Study details in generated apps

Readers can open any study in the list of effect sizes to see its reference, DOI
link and, where permitted, its abstract. Authors supply these details when they
build the app; generated apps never contact external services.

![Desktop: an expanded study with a CC BY abstract, and a second study whose abstract is withheld](desktop.png)

![Narrow (390 px): the same details wrap within the visible table width](narrow.png)

## Design

- **Source.** `fetch_study_metadata()` queries the Crossref REST API at build time,
  once per distinct DOI. It needs no key, adds no package dependency (base
  `utils::download.file()` plus `jsonlite`) and sends an optional `mailto` from
  `METAUI_MAILTO`, as Crossref's etiquette asks. Results can be cached in an editable
  CSV. Missing, malformed, unknown (HTTP 404), unreachable and conflicting DOIs are
  reported by status. They receive no metadata, and nothing is matched by title.
  Authors can also write the CSV themselves.
- **Abstracts.** Crossref states that its metadata is mostly CC0 facts, "with the
  exception of abstracts which are subject to publisher or author copyright"
  ([Crossref, retrieving metadata](https://www.crossref.org/documentation/retrieve-metadata/)).
  The Initiative for Open Abstracts says third parties may not republish an abstract
  "unless the license under which it is originally published permits such
  republication" ([I4OA](https://i4oa.org/)). OpenAlex does not ship plain-text
  abstracts "for legal reasons"
  ([OpenAlex work attributes](https://help.openalex.org/data/works/attributes/)).
  The default therefore keeps an abstract only when the Crossref record declares a
  CC BY, CC BY-SA, CC0 or public-domain-mark licence. The licence must apply to the
  version of record or an unspecified version and must already have started. NC/ND
  licences, text-mining licences and embargoed licences do not qualify. The app
  shows the licence link and source beside the abstract and notes that formatting
  was removed. `abstracts = "all"` is an explicit
  author opt-in, and the app then attributes the inclusion to the app authors. This
  is a conservative design rule, not legal advice. No source states outright that an
  article's CC licence covers its abstract, and neither Crossref nor OpenAlex
  verifies deposited licence values.
- **Display.** Each study with details becomes a native `<button>` in the Study
  column, with `aria-expanded`, a ▸/▾ marker matching the app's other disclosures
  and a one-line reference as its hover title. Activating it with a pointer, Enter
  or Space opens a DataTables child row under the effect. On wide screens the text
  is capped at 75 characters per line; on narrow screens it wraps to the visible
  table width and stays visible while the table scrolls sideways. Studies without
  details keep plain, escaped labels. All values are escaped when the app is built.

## Checks

- `tests/testthat/test-study-info.R` uses Crossref-shaped responses with no network
  access. It covers parsing, the licence rule, statuses, cache reuse, escaping,
  storage, removal on rebuild and `build_app()` schema-version handling. The full
  suite passes, and the template-sync test covers the new helpers.
- `demo-build.R` builds synthetic effects for ten studies, eight with real DOIs.
  A live lookup on 2026-10-30 matched 8 of 10 studies. Three CC BY abstracts were
  kept. Two abstracts were withheld because no open licence was recorded (Science;
  Psychological Science), and two works had no deposited abstract. A Scandinavian
  Journal of Work, Environment & Health article was withheld because its Crossref
  record lacks a licence, although OpenAlex lists it as CC BY. One unregistered DOI
  was reported as `not_found`, and the row without a DOI as `no_doi`.
- `browser-check.cjs` drives that app in Chromium at 1440×1000 and 390×844. It opens
  a study by keyboard (Enter) and closes it with Space, checks `aria-expanded`,
  confirms that the toggle does not select the table row, and saves these
  screenshots. No page errors occurred. The existing `tools/browser-smoke.cjs`
  also passes on an app without study details.
