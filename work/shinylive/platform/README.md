# Platform startup investigation (workstream C)

Scratch, package library, and four candidate sites: `/tmp/shinylive-speed/C/`.
The baseline is the unchanged site at `http://127.0.0.1:8946/site/dannheim/`.
All startup comparisons use `work/shinylive/timed-run.sh` and its machine-wide lock.

## Initial profile

Instrumentation of webR 0.6.0's `loadWebAssemblyModule()` separates synchronous
`new WebAssembly.Module`, `new WebAssembly.Instance`, and `postInstantiation`.
The raw page/worker console events are in `profile.json`. This is an instrumented
run under heavy concurrent machine load, not a speed comparison.

Across 56 loaded modules: compilation 0.211 s; instantiation 0.132 s;
post-instantiation 48.964 s. stringi: 0.045 / 0.062 / 24.423 s;
sass: 0.026 / 0.028 / 12.755 s; readr: 0.003 / 0.003 / 2.464 s;
lme4: 0.007 / 0.008 / 0.947 s; Matrix: 0.010 / 0.0004 / 0.018 s.
Thus the explicit compile/instantiate calls account for only 0.7% of measured
side-module loading. Post-instantiation includes symbol relocation, GOT/table
updates, and static constructors; it must not be labelled compilation or ICU
loading without further instrumentation.

`Module.mountImageUrl` totals 18.994 s, including base and app images; the
`.mount_vfs_images` phase is 18.155 s. R startup/mounting is serial. Instrumented
namespace startup is slower than the earlier 47–49 s baseline because machine
load was substantially higher. This profile cannot be used as the denominator
for an optimization speedup.

## Packaging and configuration findings

- The 92 package images all belong to the app's actual Depends/Imports closure
  (including optional bayesmeta explicitly referenced in the model file). A
  closure computed from exported WebAssembly DESCRIPTION files and the app's
  renv-discovered roots excludes zero bundled packages. The original generated
  app checks all 28 namespaces at startup, so lazy namespace mounting alone
  would merely move these same mounts into those checks.
- `shinylive-sw.js` ships `useCaching = false`. With it enabled, shinylive has
  a cache-first path for its `/shinylive/` resources, including R.wasm and
  package images. Its default cache version follows shinylive's version, not
  the application package contents, so blindly enabling it risks stale assets
  after re-exporting an updated app under the same URL.
- Assets 0.10.12 use PostMessage unless cross-origin isolation is available;
  automatic channel selection then permits SharedArrayBuffer. Ordinary static
  hosting does not consistently provide COOP/COEP headers, so requiring that
  changes the hosting contract.
- `baseUrl` identifies the self-contained local webR assets. Changing it to a
  CDN adds a runtime network dependency. `max_filesize` is an export inclusion
  limit, not a speed setting: too small excludes a package from the bundle.
  `assets_version` and `template_dir` select the asset/template versions.
- On 2026-10-30 the GitHub release API reports shinylive 0.10.16 and webR 0.6.0
  as latest. shinylive 0.10.16 still pins webR ^0.6.0; its release notes concern
  Python upgrades. No newer released R runtime was available to compare.
  Sources: https://github.com/posit-dev/shinylive/releases/tag/v0.10.16,
  https://github.com/r-wasm/webr/releases/tag/v0.6.0,
  https://github.com/posit-dev/shinylive/blob/v0.10.16/package.json.


## Finer profile and candidate

A second instrumented run (`post-profile.json`) splits the first part of
post-instantiation: table updates total 0.008 s, GOT updates 0.066 s, and
`relocateExports` totals 44.937 s. Relocation alone: stringi 18.332 s, sass
14.507 s, readr 3.026 s, lme4 1.050 s; Matrix effectively zero.

webR's `isImmutableGlobal` tests mutability by assigning to a global and catching
TypeError. These exceptions are an expected part of relocation. The candidate
suppresses unused error-stack capture around that synchronous loop with
`Error.stackTraceLimit = 0`, restoring the previous value in `finally`.
The same assignments, exceptions, relocation offsets and returned exports are
retained. The candidate does not change R package binaries or app source.

The patch is exact-match guarded and idempotent. An unrecognized loader is
left unchanged with a warning. This depends on webR's generated JS internals;
future webR/Emscripten releases need profiling again. Unexpected relocation
errors would have less JavaScript stack detail, but the setting is restored
before the error propagates. Browser engines that do not implement
`Error.stackTraceLimit` may see no benefit.

## Measurement interruption

The box developed severe concurrent resource pressure during the first paired
comparison: load average peaked above 150 on four cores, all 4 GB swap was
occupied, and memory pressure reported over 35% full stalls. The unchanged
baseline failed with a 30-second `locator.click('#go')` timeout after startup.
That failed run provides no valid baseline time. Queued exploratory runs were
reduced; only owned processes were stopped. No unrelated process or tailnet
configuration was touched.

## Reproduction

Run scripts only on a disposable scratch export. `instrument.py` saves the
unmodified loader and shinylive script into C scratch; do not instrument the
read-only baseline or the shared asset cache. `post-profile.py` replaces the
loader instrumentation with the finer relocation profile. Both are exploratory
scripts for this explicitly pinned asset version.

```sh
python3 work/shinylive/platform/instrument.py /tmp/shinylive-speed/C/site
flock /tmp/shinylive-speed-timing.lock env \
  URL=http://127.0.0.1:8981/dannheim/ OUT=/tmp/shinylive-speed/C/profile.json \
  node work/shinylive/platform/profile.cjs
python3 work/shinylive/platform/profile-summary.py work/shinylive/platform/profile.json
node work/shinylive/platform/verify-relocation.cjs /tmp/shinylive-speed/C/table/shinylive/webr/R.js
```

The script named `prepare-parallel.py` records the exploratory precompile
prototype, not a recommended export feature: it duplicates compiled modules
as separately served files and compiles all of them concurrently before boot.
Its theoretical saving is small given the measured 0.211 seconds spent in
explicit compilation. It was not integrated into `export_static()`.

## Validation status and handoff

**The helper is experimental and is deliberately not called by
`export_static()` yet.** Apply it explicitly to a disposable export with
`metaUI:::static_optimize_webr(site_dir)`. The candidate served at port 8984
was patched using the installed helper from this worktree. The initial Dannheim
build/export used the prescribed isolated C library and unchanged example.

Passed: existing export tests (7 expectations, no failures/warnings; network
integration test skipped); direct execution of the patched loader's actual
relocation function verifies immutable offset arithmetic, mutable-global and
function identity, and restoration of the error-stack limit on success and
failure. `git diff --check` passes.

**Not completed:** a valid paired timing comparison, cache/precompile timings,
full static smoke, and workbook parity. The initial baseline failed its click
timeout during resource exhaustion. The later smoke run reached page load but
stalled before the iframe was created; memory full-stall pressure subsequently
reached 74% and load exceeded 60 again. Owned smoke/comparison processes were
stopped rather than leave stalled browsers holding the shared timing lock.
No successful timing ratio, numerical-parity claim, or 2x-speedup claim can be
made from these runs. There is no measured option recommended for integration.

Next validation, when the box has memory and CPU capacity: call the helper on
the new export; alternate the unchanged baseline and candidate through
`timed-run.sh` with two visits per context; compare cold and repeat medians and
load averages separately. Then run `static-smoke.cjs` with MODE=static and
compare its workbook to `work/shinylive/evidence/workbook-server.xlsx` using
`compare-workbooks.R`. Only then enable the helper in `export_static()`.

The reason Chromium did not reuse its ordinary HTTP cache in the original
baseline remains unresolved. Disabled Cache Storage is a separate finding,
not sufficient evidence to explain those HTTP-cache misses. Compiled `.so`
module caching (for example IndexedDB structured cloning) was not implemented:
explicit compilation is already below 1% of the measured side-module work.
ICU data reads and ordinary R-level namespace execution were not separately
profiled; the recorded relocation measurements establish that the large
stringi/sass delays are not predominantly explicit Wasm compilation.
