# Fast route (variance components held fixed) vs full refits, leaving out one study at a time, on
# selections of the Diversity data (Wallrich et al., 2024). Run from the package root, with the
# Diversity app built by metaUI (its dataset.rds and models.R) in METAUI_DIVERSITY_APP:
#   Rscript validation/leave-one-out/compare.R subsets   # 8-120 studies, 3 seeds x 2 models
#   Rscript validation/leave-one-out/compare.R full      # all 646 studies, CHE model (about 1 h on 4 cores)
# One row per selection and model is written next to this script as compare-<set>.csv.
suppressPackageStartupMessages({library(metafor); library(metaUI)})
args <- commandArgs(TRUE); which_set <- args[1]
app <- Sys.getenv("METAUI_DIVERSITY_APP", "work-local/diversity")
ns <- asNamespace("metaUI")
df_all <- readRDS(file.path(app, "dataset.rds"))
env <- new.env(); library(magrittr); sys.source(file.path(app, "models.R"), env)
models <- as.data.frame(env$models_to_run)
specs <- list(CHE = models[models$name == "Correlated and Hierarchical Effects Model (as in paper)", ],
  ML = models[models$name == "Random-Effects Multilevel Model", ])
studies_all <- unique(as.character(df_all$metaUI__study_id))
sets <- if (which_set == "full") list(list(name = "all646", model = "CHE", studies = studies_all)) else {
  out <- list()
  for (size in c(8, 15, 30, 60, 120)) for (seed in 1:3) for (m in c("CHE", "ML")) {
    set.seed(seed * 1000 + size); out[[length(out) + 1]] <- list(name = paste0("k", size, "_s", seed), model = m, studies = sample(studies_all, size))
  }
  out
}
bounds <- c(.02, .05, .1)
cores <- as.integer(Sys.getenv("CORES", "3"))
results <- lapply(sets, function(set) {
  df <- df_all[as.character(df_all$metaUI__study_id) %in% set$studies, ]
  spec <- specs[[set$model]]
  t_full <- system.time(full <- ns$metaUI_fit_models(df, spec))[["elapsed"]]
  loo <- full$loo[[1]]
  if (!is.null(loo$reason)) return(list(set = set$name, model = set$model, reason = loo$reason))
  t_fast <- system.time(fast <- ns$metaUI_loo_estimates(loo))[["elapsed"]]
  studies <- unique(as.character(df$metaUI__study_id))
  slow <- parallel::mclapply(studies, function(s) {
    t <- system.time(r <- ns$metaUI_fit_models(df[as.character(df$metaUI__study_id) != s, ], spec)$table)[["elapsed"]]
    data.frame(study = s, es = r$fit_es, LCL = r$fit_LCL, UCL = r$fit_UCL, status = r$status, secs = t)
  }, mc.cores = cores)
  slow <- do.call(rbind, slow)
  fast <- fast[match(slow$study, fast$study), ]
  ref <- unlist(full$table[1, c("fit_es", "fit_LCL", "fit_UCL")]); names(ref) <- c("es", "LCL", "UCL")
  fz <- ns$metaUI_leave_one_out(fast, ref, df); sz <- ns$metaUI_leave_one_out(transform(slow, effects = 1), ref, df)
  sz <- sz[match(fz$study, sz$study), ]
  shown <- function(x) ns$metaUI_show(x, df)
  flags <- sapply(bounds, function(b) {
    f <- ns$metaUI_leave_one_out(fast, ref, df, b); s <- ns$metaUI_leave_one_out(transform(slow, effects = 1), ref, df, b)
    s <- s[match(f$study, s$study), ]; sum(f$crosses_bound != s$crosses_bound) })
  list(set = set$name, model = set$model, studies = length(studies), effects = nrow(df), full_es = shown(ref[["es"]]),
    full_fit_s = t_full, fast_s = t_fast, refit_median_s = median(slow$secs), refit_total_s = sum(slow$secs), failed = sum(slow$status != "ok"),
    max_d_es = max(abs(fz$es - sz$es), na.rm = TRUE), max_d_ci = max(abs(c(fz$LCL - sz$LCL, fz$UCL - sz$UCL)), na.rm = TRUE),
    max_change_slow = max(abs(sz$change), na.rm = TRUE), max_rel_err = max(abs(fz$change - sz$change), na.rm = TRUE) / max(abs(sz$change), na.rm = TRUE),
    sig_flags_fast = sum(fz$significance_changes, na.rm = TRUE), sig_flags_slow = sum(sz$significance_changes, na.rm = TRUE), sig_disagree = sum(fz$significance_changes != sz$significance_changes, na.rm = TRUE),
    bound_disagree = paste(flags, collapse = "/"), top_same = fz$study[1] == sz$study[order(-abs(sz$change))][1],
    rank_cor = suppressWarnings(cor(abs(fz$change), abs(sz$change), method = "spearman")))
})
summary <- dplyr::bind_rows(lapply(results, as.data.frame))
utils::write.csv(summary, file.path("validation", "leave-one-out", paste0("compare-", which_set, ".csv")), row.names = FALSE)
print(summary)
