# Bayesian model: grid quadrature

The opt-in Bayesian model (normal-normal random effects on study-level effects, flat
prior on the mean μ, half-normal prior on the between-study SD τ) is computed by
`metaUI_bayesian_grid()` in `R/bayesian.R`, not by `bayesmeta`.

## Method

With μ's flat prior integrated out, the marginal posterior of τ is known up to a
constant, and μ given τ is exactly normal. The posterior of μ is therefore a normal
mixture over τ. The function evaluates the τ posterior on a grid that is uniform in
x = log(τ + τ0), with τ0 one tenth of the smaller of the smallest SE and the prior
scale. This resolves both a peak at τ = 0 and a long right tail.

1. A first pass of 1,000 points runs from τ = 0 to an upper bound of 10 × the
   largest of the prior scale, the range of the effects and the smallest SE. The
   bound is multiplied by four until the density there is below e^-40 of its peak.
2. The second pass places 2,000 points over the region where the density is within
   e^-40 of its peak (plus one first-pass cell on each side). If that region covers
   fewer than a quarter of the new points (a very narrow posterior), the grid zooms
   in again, up to ten times.
3. Trapezoid weights give the mixture weights. μ quantiles are found by root-finding
   on the exact mixture CDF. τ quantiles come from the cumulative trapezoid CDF,
   interpolated linearly in x.

## Validation (November 2026)

Full report, scripts and raw results: `work/review-2026-11/bayes-grid/` (not tracked).

- **Datasets:** 412 simulated datasets with k = 2, 3, 5, 10, 20, 50, 100 and 646
  studies. True τ was 0, 0.5 or 3 × the median SE. SEs were moderate or spread 1:100.
  Effects were on the SMD or Fisher z scale. The prior scale was 0.05, 0.5, 1, 2 or
  10 times the default. Some datasets had a 6-SD outlier.
- **Real data:** Coles et al. (2019; 97 studies) and the Diversity dataset (646
  studies).
- **Reference:** an independent computation using `stats::integrate()` on about 100
  geometric τ segments (relative tolerance 1e-11), with root-finding for quantiles.
  It agrees with bayesmeta's τ quantiles to 4e-10 posterior SDs.

Largest error against the reference, in posterior SDs:

| Quantity | Grid (412 datasets) | bayesmeta 3.5 (338 datasets) |
|---|---:|---:|
| μ mean, median, 2.5/5/95/97.5% quantiles | 7e-8 | 1e-2 |
| τ mean, median, 2.5/97.5% quantiles | 2e-4 (relative 2e-5) | 4e-10 |

- **Coverage:** every grid quantity agrees with the reference to three significant
  digits on the reported scale (r for correlations). For bayesmeta's μ quantiles the
  share is 65–98%.
- **bayesmeta failures:** bayesmeta failed ("integral is probably divergent") on 2
  of 340 datasets (k = 20, large τ, SE ratio 1:100).
- **Grid settings:** results change by less than 6e-4 posterior SDs when the density
  is halved or doubled (n = 1000 or 4000) or the range is widened (cut e^-80). The
  first-pass mass outside the final grid was at most 1e-18.
- **Real datasets:** grid errors were within 3e-5 SDs for Coles and 2e-5 SDs for
  Diversity.
- **Edge cases (external review):** the weighted residual sum of squares is computed
  from explicit residuals, which stays accurate when one study's SE is far smaller
  than the others' (y = 0, 10, 10 with SEs 1e-9, 1, 1 matches the reference to five
  digits). Posterior SDs are computed around the mean, so they do not depend on the
  location of the effects. A posterior narrower than one first-pass cell (two studies
  with SE 1e-14 and prior scale 1e-10) is resolved over hundreds of points and stable
  when the density is raised tenfold. These cases are in the tests.

## Speed

Median seconds per fit on Ubuntu 24.04 with R 4.6. Native grid times are from `tools/benchmark-bayesian.R` on an idle machine; bayesmeta times were measured during the validation run, which loaded the machine more heavily:

| Studies | Grid (native) | Grid (webR, Node) | bayesmeta |
|---:|---:|---:|---:|
| 20 | 0.008 s | 0.09 s | 16 s (median), up to 48 s |
| 50 | 0.01 s | — | 59 s (median), up to 75 s |
| 97 (Coles) | 0.10 s | — | 510 s |
| 100 | 0.013 s | 0.15 s | not run |
| 646 | 0.06 s | 0.29 s | not run |
| 2,000 | 0.11 s | — | not run |

Memory is O(grid size). Each step loops over studies with vectors of length 2,000.
`tools/benchmark-bayesian.R` reproduces the native timings.

The study cap (default 20, maximum 50) is removed. `max_studies` remains as an
optional author cap. The reproduction script copies `metaUI_bayesian_grid()`, so it
reproduces the app's Bayesian row exactly without bayesmeta.
