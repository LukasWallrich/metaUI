# Run from the repository root with metaUI dependencies and meta installed.
# Independent check of M's helpers against meta 8.5.0 on fresh simulations (own generator, seed 77).
source("inst/template_code/analysis.R")
stopifnot(as.character(utils::packageVersion('meta')) == '8.5.0')
args <- commandArgs(TRUE)
output <- if (length(args)) args[1] else Sys.getenv('METAUI_INDEPENDENT_OUT', '')
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
  failed <- vapply(list(m, h, r1, r2), inherits, logical(1), "err")
  category <- if (!any(failed)) "paired" else if (identical(failed, c(TRUE, TRUE, FALSE, FALSE)))
    "both_trimfill_error" else if (identical(failed, c(TRUE, FALSE, TRUE, TRUE)))
    "meta_trimfill_error_helper_converged" else "unexpected_error"
  # Accept only trimfill convergence failures, never arbitrary errors.
  convergence <- function(x) inherits(x, "err") && grepl("converg|convergenc|Fisher scoring", x$msg, ignore.case = TRUE)
  if (category == "both_trimfill_error" && !(convergence(m) && convergence(h))) category <- "unexpected_error"
  if (category == "meta_trimfill_error_helper_converged" && !(convergence(m) && convergence(r1) && convergence(r2))) category <- "unexpected_error"
  out[[i]] <- data.frame(dataset = i, k = length(y),
    k0_meta = if (failed[1]) NA else m$k0, k0_new = if (failed[2]) NA else h$k0,
    d_tf = if (any(failed[1:2])) NA else max(abs(c(m$TE.random - h$b, m$lower.random - h$ci.lb, m$upper.random - h$ci.ub))),
    d_reml = if (any(failed[3:4])) NA else max(abs(c(r1$TE.random - r2$b, r1$lower.random - r2$ci.lb, r1$upper.random - r2$ci.ub))),
    category = category, err = paste(vapply(list(m,h,r1,r2), function(x) if (inherits(x,"err")) x$msg else "", character(1)), collapse = " | "))
}
r <- do.call(rbind, out)
if (nzchar(output)) write.csv(r, output, row.names = FALSE)
print(table(r$category))
print(r[r$category != "paired", c("dataset", "category", "err")], row.names = FALSE)
cat("datasets:", nrow(r), " with imputation (meta):", sum(r$k0_meta > 0, na.rm = TRUE), " k0 mismatches:", sum(r$k0_meta != r$k0_new, na.rm = TRUE), "\n")
cat("max |diff| trim-and-fill:", signif(max(r$d_tf, na.rm = TRUE), 3), "  REML/HK:", signif(max(r$d_reml, na.rm = TRUE), 3), "\n")
paired <- r$category == "paired"
stopifnot(!any(r$category == "unexpected_error"),
  identical(r$dataset[r$category == "both_trimfill_error"], 151L),
  identical(r$dataset[r$category == "meta_trimfill_error_helper_converged"], c(128L, 272L, 382L)),
  all(is.finite(r$d_tf[paired])), all(is.finite(r$d_reml[r$category != "meta_trimfill_error_helper_converged"])),
  all(r$k0_meta[paired] == r$k0_new[paired]),
  all(r$d_tf[paired] <= 1e-8), all(r$d_reml[is.finite(r$d_reml)] <= 1e-8))
