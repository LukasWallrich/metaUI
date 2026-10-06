# GPL-3-or-later; trusted editable R; standalone specifications.
 models_to_run <- tibble::tribble(
  ~name,                                             ~aggregated, ~es,                                  ~LCL,                  ~UCL,                  ~k,               
   "Random-Effects Multilevel Model",                 FALSE,       "mod$b",                              "mod$ci.lb",           "mod$ci.ub",           "mod$k",         
   "Robust Variance Estimation",                      FALSE,       "as.numeric(mod$reg_table$b.r)",      "mod$reg_table$CI.L",  "mod$reg_table$CI.U",  "length(mod$k)", 
   "Trim-and-fill",                                   TRUE,        "mod$TE.random",                      "mod$lower.random",    "mod$upper.random",    "mod$k",         
   "P-uniform star",                                  TRUE,        "mod$est",                            "mod$ci.lb",           "mod$ci.ub",           "mod$k",         
   "Hedges-Vevea Selection Model",                    TRUE,        "as.numeric(mod$output_adj$par[2])",  "mod$ci.lb_adj[2]",    "mod$ci.ub_adj[2]",    "mod$k",         
   "Precision Effect Test",                           TRUE,        "as.numeric(mod$coefficients[1])",    "confint(mod)[1, 1]",  "confint(mod)[1, 2]",  "mod$df+2",      
   "Precision Effect Estimate using Standard Error",  TRUE,        "as.numeric(mod$coefficients[1])",    "confint(mod)[1, 1]",  "confint(mod)[1, 2]",  "mod$df+2"
)


 models_code <- tibble::tribble(
  ~name,                                             ~code,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
   "Random-Effects Multilevel Model",                 "metaUI_multilevel_fit(df)",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
   "Robust Variance Estimation",                      "metaUI_rve_fit(df)",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
   "Trim-and-fill",                                   "meta::trimfill(meta::metagen(
                                            TE = metaUI__effect_size,
                                            seTE = metaUI__se,
                                            data = df,
                                            studlab = df$metaUI__study_id,
                                            common = FALSE,
                                            random = TRUE,
                                            method.tau = \"ML\", # as recommended by  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4950030/
                                            method.random.ci = \"HK\",
                                            prediction = TRUE,
                                            sm = df$metaUI__es_type[1]
                                        ))", 
   "P-uniform star",                                  "puniform::puni_star(
                    yi = df$metaUI__effect_size, vi = df$metaUI__variance,
                    alpha = .05,
                    side = \"right\", method = \"ML\", boot = FALSE
                    )",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
   "Hedges-Vevea Selection Model",                    "weightr::weightfunct(df$metaUI__effect_size,
                df$metaUI__variance, steps = c(0.025, 1), fe = FALSE)",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
   "Precision Effect Test",                           "lm(metaUI__effect_size ~ sqrt(metaUI__variance), data = df, weights = 1 / metaUI__variance)",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
   "Precision Effect Estimate using Standard Error",  "lm(metaUI__effect_size ~ metaUI__variance, data = df, weights = 1 / metaUI__variance)"
)


# Default specifications; source-reference row appended below. Helper functions etc must be added above
models_to_run <- models_to_run %>% dplyr::left_join(models_code, by = "name")
models_to_run
# Source methodological reference on rounded extracted g/SE; GPL-3-or-later.
# One author-composited effect per study, no averaging across outcome pools.
source_model <- tibble::tibble(
  name = "Source-reference: REML / Hartung-Knapp (rounded inputs)", aggregated = FALSE,
  es = "mod$TE.random", LCL = "mod$lower.random", UCL = "mod$upper.random", k = "mod$k",
  code = 'if (anyDuplicated(df$metaUI__study_id)) stop("Source-reference requires one effect per independent study.")
    meta::metagen(TE = df$metaUI__effect_size, seTE = df$metaUI__se,
    studlab = df$metaUI__study_id, sm = "SMD", common = FALSE, random = TRUE,
    method.tau = "REML", method.random.ci = "HK", adhoc.hakn.ci = "", level = .95, level.ma = .95)'
)
models_to_run <- dplyr::bind_rows(models_to_run, source_model)
