Conditions run: 412; reference 412, grid 412, bayesmeta 340. Failures: reference 0, grid 0, bayesmeta 2

- bayesmeta failed on dataset 120: the integral is probably divergent
- bayesmeta failed on dataset 210: the integral is probably divergent

### A. Accuracy against the adaptive-integration reference, all datasets

| group | datasets | max_abs | max_rel | max_by_sd | max_abs_reported | sig3_agree |
| --- | --- | --- | --- | --- | --- | --- |
| bayesmeta / mu | 338 | 7.4e-03 | 3.2e-01 | 9.9e-03 | 7.4e-03 | 86.9% |
| grid / mu | 412 | 6.1e-08 | 2.8e-06 | 7.2e-08 | 6.1e-08 | 100.0% |
| bayesmeta / tau | 338 | 3.6e-12 | 6.8e-09 | 3.8e-10 | 3.6e-12 | 100.0% |
| grid / tau | 412 | 1.9e-04 | 2.2e-05 | 1.9e-04 | 1.9e-04 | 100.0% |

### B. By quantity (grid vs reference, all 412 datasets)

| group | datasets | max_abs | max_rel | max_by_sd | max_abs_reported | sig3_agree |
| --- | --- | --- | --- | --- | --- | --- |
| mu_mean | 412 | 1.2e-09 | 8.9e-08 | 2.4e-08 | 1.2e-09 | 100.0% |
| mu_median | 412 | 1.7e-09 | 4.3e-08 | 2.4e-08 | 1.7e-09 | 100.0% |
| mu_q025 | 412 | 6.1e-08 | 4.4e-07 | 5.7e-08 | 6.1e-08 | 100.0% |
| mu_q05 | 412 | 5.3e-08 | 2.8e-06 | 4.8e-08 | 5.3e-08 | 100.0% |
| mu_q95 | 412 | 5.3e-08 | 2.4e-08 | 6.7e-08 | 5.3e-08 | 100.0% |
| mu_q975 | 412 | 6.1e-08 | 2.3e-08 | 7.2e-08 | 6.1e-08 | 100.0% |
| tau_mean | 412 | 4.6e-08 | 9.2e-08 | 1.0e-07 | 4.6e-08 | 100.0% |
| tau_median | 412 | 2.5e-06 | 2.0e-06 | 9.0e-06 | 2.5e-06 | 100.0% |
| tau_q025 | 412 | 7.1e-06 | 2.2e-05 | 5.9e-05 | 7.1e-06 | 100.0% |
| tau_q975 | 412 | 1.9e-04 | 2.0e-05 | 1.9e-04 | 1.9e-04 | 100.0% |

### C. By quantity (bayesmeta vs reference, 338 datasets)

| group | datasets | max_abs | max_rel | max_by_sd | max_abs_reported | sig3_agree |
| --- | --- | --- | --- | --- | --- | --- |
| mu_mean | 338 | 4.6e-04 | 3.2e-02 | 4.5e-03 | 4.6e-04 | 98.2% |
| mu_median | 338 | 4.7e-04 | 2.5e-02 | 4.7e-03 | 4.7e-04 | 97.3% |
| mu_q025 | 338 | 7.4e-03 | 1.9e-01 | 8.2e-03 | 7.4e-03 | 65.1% |
| mu_q05 | 338 | 3.8e-03 | 3.2e-01 | 7.3e-03 | 3.8e-03 | 76.0% |
| mu_q95 | 338 | 3.7e-03 | 2.4e-02 | 9.2e-03 | 3.7e-03 | 94.4% |
| mu_q975 | 338 | 7.3e-03 | 1.4e-02 | 9.9e-03 | 7.3e-03 | 90.5% |
| tau_mean | 338 | 2.0e-15 | 2.9e-15 | 2.9e-14 | 2.0e-15 | 100.0% |
| tau_median | 338 | 3.6e-12 | 3.3e-10 | 3.8e-10 | 3.6e-12 | 100.0% |
| tau_q025 | 338 | 3.5e-12 | 6.8e-09 | 3.7e-10 | 3.5e-12 | 100.0% |
| tau_q975 | 338 | 3.5e-12 | 6.5e-11 | 2.5e-10 | 3.5e-12 | 100.0% |

### D. Grid vs reference by number of studies

| group | datasets | max_abs | max_rel | max_by_sd | max_abs_reported | sig3_agree |
| --- | --- | --- | --- | --- | --- | --- |
| k = 002 | 60 | 1.9e-04 | 2.0e-05 | 7.9e-05 | 1.9e-04 | 100.0% |
| k = 003 | 60 | 7.2e-05 | 1.7e-05 | 8.7e-05 | 7.2e-05 | 100.0% |
| k = 005 | 72 | 2.0e-05 | 2.0e-05 | 8.2e-05 | 2.0e-05 | 100.0% |
| k = 010 | 72 | 2.2e-05 | 2.2e-05 | 9.4e-05 | 2.2e-05 | 100.0% |
| k = 020 | 72 | 8.5e-06 | 2.0e-05 | 1.9e-04 | 8.5e-06 | 100.0% |
| k = 050 | 4 | 2.1e-06 | 8.0e-06 | 4.8e-05 | 2.1e-06 | 100.0% |
| k = 100 | 36 | 2.8e-06 | 8.3e-06 | 4.5e-05 | 2.8e-06 | 100.0% |
| k = 646 | 36 | 2.0e-06 | 1.8e-05 | 1.7e-04 | 2.0e-06 | 100.0% |

### E. Grid vs reference by condition (k <= 50)

| group | datasets | max_abs | max_rel | max_by_sd | max_abs_reported | sig3_agree |
| --- | --- | --- | --- | --- | --- | --- |
| true tau: large | 101 | 1.2e-04 | 2.0e-05 | 1.9e-04 | 1.2e-04 | 100.0% |
| true tau: small | 120 | 1.9e-04 | 2.2e-05 | 8.9e-05 | 1.9e-04 | 100.0% |
| true tau: zero | 119 | 1.0e-04 | 1.7e-05 | 8.2e-05 | 1.0e-04 | 100.0% |
| SEs: extreme | 151 | 1.9e-04 | 2.2e-05 | 9.4e-05 | 1.9e-04 | 100.0% |
| SEs: moderate | 189 | 5.3e-05 | 2.0e-05 | 1.9e-04 | 5.3e-05 | 100.0% |
| scale: Fisher z | 169 | 9.8e-05 | 2.0e-05 | 1.4e-04 | 9.8e-05 | 100.0% |
| scale: SMD | 171 | 1.9e-04 | 2.2e-05 | 1.9e-04 | 1.9e-04 | 100.0% |
| prior x  0.05 | 72 | 3.5e-06 | 2.0e-05 | 1.9e-04 | 3.5e-06 | 100.0% |
| prior x  0.50 | 60 | 1.0e-05 | 1.7e-05 | 9.4e-05 | 1.0e-05 | 100.0% |
| prior x  1.00 | 76 | 1.6e-05 | 1.7e-05 | 9.0e-05 | 1.6e-05 | 100.0% |
| prior x  2.00 | 60 | 3.4e-05 | 1.7e-05 | 9.4e-05 | 3.4e-05 | 100.0% |
| prior x 10.00 | 72 | 1.9e-04 | 2.2e-05 | 8.9e-05 | 1.9e-04 | 100.0% |
| outlier: FALSE | 304 | 1.9e-04 | 2.2e-05 | 1.9e-04 | 1.9e-04 | 100.0% |
| outlier: TRUE | 36 | 2.2e-05 | 1.9e-05 | 8.9e-05 | 2.2e-05 | 100.0% |

### F. Grid vs bayesmeta directly (datasets with both)

| quantity | max_abs | max_by_sd |
| --- | --- | --- |
| mu_mean | 4.6e-04 | 4.5e-03 |
| mu_median | 4.7e-04 | 4.7e-03 |
| mu_q025 | 7.4e-03 | 8.2e-03 |
| mu_q05 | 3.8e-03 | 7.3e-03 |
| mu_q95 | 3.7e-03 | 9.2e-03 |
| mu_q975 | 7.3e-03 | 9.9e-03 |
| tau_mean | 4.6e-08 | 1.0e-07 |
| tau_median | 2.5e-06 | 9.0e-06 |
| tau_q025 | 7.1e-06 | 5.9e-05 |
| tau_q975 | 1.9e-04 | 1.9e-04 |

### G. Grid robustness: change from the default grid (n = 2000, cut = 40), max over all datasets, in posterior SDs

| variant | mu_max | tau_max |
| --- | --- | --- |
| cut = 80 (wider range) | 1.7e-08 | 1.9e-04 |
| n = 1000 (half density) | 2.2e-07 | 5.1e-04 |
| n = 4000 (double density) | 5.4e-08 | 1.5e-04 |

### H. Grid range and dropped tail mass

Largest share of first-pass posterior mass outside the final grid: 1.3e-18 

Final grid starts at tau = 0 in 81% of datasets; elsewhere the posterior below the grid's start is < e^-40 of its peak.

### I. Timing (seconds, this machine while other validation jobs ran)

| k | grid_median | grid_max | bayesmeta_median | bayesmeta_max |
| --- | --- | --- | --- | --- |
| 2 | 0.008 | 0.091 | 2.1 | 3.8 |
| 3 | 0.008 | 0.195 | 2.5 | 5.1 |
| 5 | 0.009 | 0.156 | 3.0 | 13.3 |
| 10 | 0.011 | 0.451 | 5.3 | 20.1 |
| 20 | 0.009 | 0.154 | 15.5 | 48.3 |
| 50 | 0.011 | 0.013 | 58.6 | 74.8 |
| 100 | 0.017 | 0.134 | not run |  |
| 646 | 0.050 | 0.175 | not run |  |

### J. Worst grid cases (largest error in posterior SDs)

| id | k | scale | tau | se | prior | outlier | quantity | ref | est | by_sd |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 15 | 20 | SMD | large | moderate | 0.05 | FALSE | tau_q975 | 0.173132 | 0.173135 | 1.9e-04 |
| 344 | 646 | SMD | small | moderate | 0.05 | FALSE | tau_q975 | 0.110232 | 0.110234 | 1.7e-04 |
| 45 | 20 | Fisher z | large | moderate | 0.05 | FALSE | tau_q975 | 0.0799195 | 0.079921 | 1.4e-04 |
| 119 | 10 | Fisher z | large | extreme | 0.5 | FALSE | tau_q975 | 0.265023 | 0.265028 | 9.4e-05 |
| 225 | 20 | Fisher z | large | moderate | 2 | FALSE | tau_q975 | 0.291044 | 0.291049 | 9.4e-05 |
| 165 | 20 | Fisher z | large | moderate | 1 | FALSE | tau_q975 | 0.201626 | 0.201629 | 9.0e-05 |
| 329 | 10 | SMD | small | moderate | 10 | TRUE | tau_q975 | 1.14482 | 1.14484 | 8.9e-05 |
| 87 | 3 | SMD | large | extreme | 0.5 | FALSE | tau_q975 | 0.618342 | 0.618352 | 8.7e-05 |

### K. Worst bayesmeta cases

| id | k | scale | tau | se | prior | outlier | quantity | ref | est | by_sd |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 41 | 2 | Fisher z | large | moderate | 0.05 | FALSE | mu_q975 | 0.0408595 | 0.0402812 | 9.9e-03 |
| 41 | 2 | Fisher z | large | moderate | 0.05 | FALSE | mu_q95 | 0.0223438 | 0.0218036 | 9.2e-03 |
| 11 | 2 | SMD | large | moderate | 0.05 | FALSE | mu_q975 | 0.198741 | 0.197676 | 9.2e-03 |
| 3 | 5 | SMD | zero | moderate | 0.05 | FALSE | mu_q975 | 0.560027 | 0.559179 | 8.5e-03 |
| 11 | 2 | SMD | large | moderate | 0.05 | FALSE | mu_q95 | 0.161991 | 0.161032 | 8.2e-03 |

