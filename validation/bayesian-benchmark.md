# Bayesian first-fit latency

bayesmeta 3.5, system R 4.6.1, Ubuntu 24.04, default delta/epsilon/integration
accuracy, flat mean and half-normal(0.5) heterogeneity prior; no cache.
Synthetic independent-study observations with sigma between 0.1 and 0.3 and
between-study SD 0.2. The machine also ran R package checks, so wall times are
approximate and conservative; these are individual observations, not a population
95th percentile. Reproduce with `Rscript --vanilla tools/benchmark-bayesian.R bayes.csv`:
seed 90 generated the 10- and 50-study observations in sequence and seed 92 the
200-study run (120-second limit). The 20-study observation came from a separate run
whose seed was not recorded; the script uses seed 91 for it.

| Studies | Measured wall seconds | Interpretation |
|---:|---:|---|
| 10 | 3.743 | Suitable for interactive fitting |
| 20 | 19.992 (12.237 CPU) | Default cap; sensitivity lazy |
| 50 | 96.831 | Slow; author override requires build warning |
| 200 | Did not finish during the benchmark window | Unsupported for interactive authoring |

Default max_studies is 20, hard author limit 50. Opus reviewed and agreed on
these measured limits. Accuracy was not loosened. Results above the configured
cap report a reason; other model rows remain available. Sensitivity reuses the
exact baseline fit and computes the half/double prior fits only on demand.
Analyze blocks the Shiny R process (including other sessions); downloads can
request two more fits. This limitation is stated in build warnings, the manifest,
README and download help. Revisiting the hard cap needs new measurements.
