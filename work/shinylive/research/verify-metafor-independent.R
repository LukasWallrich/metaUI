# Run from the repository root with metaUI dependencies and meta installed.
# Independent check of M's helpers against meta 8.5.0 on fresh simulations (own generator, seed 77).
source("inst/template_code/analysis.R")
set.seed(77); out <- list()
for (i in 1:400) {
  k <- sample(3:40, 1); tau <- sample(c(0, .05, .3, .8), 1)
  se <- sqrt(runif(k, .005, .3)); y <- rnorm(k, sample(c(-.5, 0, .4), 1), sqrt(tau^2 + se^2))
  if (i %% 3 == 0) { keep <- y / se > 1 | runif(k) < .3; if (sum(keep) >= 3) { y <- y[keep]; se <- se[keep] } }  # selection -> asymmetry
  if (i %% 5 == 0) y <- round(y, 1)  # ties
  if (i %% 2 == 0) y <- -y
  df <- data.frame(metaUI__effect_size = y, metaUI__se = se, metaUI__variance = se^2)
  E <- function(x) tryCatch(x, error = function(e) structure(list(msg = conditionMessage(e)), class = "err"))
  m <- E(suppressWarnings(meta::trimfill(meta::metagen(TE = y, seTE = se, common = FALSE, random = TRUE,
    method.tau = "ML", method.random.ci = "HK", prediction = TRUE, sm = "SMD"))))
  h <- E(suppressWarnings(metaUI_trimfill(df)))
  r1 <- E(suppressWarnings(meta::metagen(TE = y, seTE = se, sm = "SMD", common = FALSE, random = TRUE,
    method.tau = "REML", method.random.ci = "HK", adhoc.hakn.ci = "")))
  r2 <- E(suppressWarnings(metafor::rma(yi = y, sei = se, method = "REML", test = "knha")))
  if (inherits(m, "err") || inherits(h, "err") || inherits(r1, "err") || inherits(r2, "err")) {
    out[[i]] <- data.frame(k = length(y), k0_meta = NA, k0_new = NA, d_tf = NA, d_reml = NA,
      err = paste(class(m)[1], class(h)[1], class(r1)[1], class(r2)[1], if (inherits(m, "err")) m$msg else "", if (inherits(h, "err")) h$msg else "")); next }
  out[[i]] <- data.frame(k = length(y), k0_meta = m$k0, k0_new = h$k0,
    d_tf = max(abs(c(m$TE.random - h$b, m$lower.random - h$ci.lb, m$upper.random - h$ci.ub))),
    d_reml = max(abs(c(r1$TE.random - r2$b, r1$lower.random - r2$ci.lb, r1$upper.random - r2$ci.ub))), err = "")
}
r <- do.call(rbind, out)
print(table(r$err[r$err != ""]))
cat("datasets:", nrow(r), " with imputation (meta):", sum(r$k0_meta > 0), " k0 mismatches:", sum(r$k0_meta != r$k0_new, na.rm = TRUE), "\n")
cat("max |diff| trim-and-fill:", signif(max(r$d_tf, na.rm = TRUE), 3), "  REML/HK:", signif(max(r$d_reml, na.rm = TRUE), 3), "\n")
