# Exported reproduction code runs in a fresh R process, next to the workbook of the
# same analysis, and must reproduce the app's model rows without loading metaUI.
run_export <- function(dir, script, workbook) {
  if (normalizePath(workbook) != normalizePath(file.path(dir, "analysis-metaUIdata.xlsx"), mustWork = FALSE))
    file.copy(workbook, file.path(dir, "analysis-metaUIdata.xlsx"), overwrite = TRUE)
  out <- file.path(dir, "reproduced.rds")
  writeLines(c(sprintf("source(%s)", deparse(script)),
    sprintf("saveRDS(list(estimates = estimates, agreement = agreement, selected = nrow(selected), metaUI = \"metaUI\" %%in%% loadedNamespaces()), %s)",
      deparse(out))), file.path(dir, "runner.R"))
  old <- Sys.getenv(c("R_TESTS", "R_LIBS"), unset = NA)
  Sys.unsetenv("R_TESTS"); Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  wd <- setwd(dir)
  on.exit({ setwd(wd); for (n in names(old)) if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, as.list(old[n])) })
  log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", "runner.R", "analysis-metaUIdata.xlsx"),
    stdout = TRUE, stderr = TRUE))
  if (!file.exists(out)) stop("Exported code failed:\n", paste(log, collapse = "\n"))
  c(readRDS(out), list(log = log))
}

copy_download <- function(path, dir, name) { file.copy(path, file.path(dir, name), overwrite = TRUE); file.path(dir, name) }

expect_reproduces <- function(result, app) {
  expect_false(result$metaUI)
  expect_true(all(result$agreement$agrees), info = paste(result$log, collapse = "\n"))
  expect_false(any(grepl("^Warning|differs? from", result$log)), info = paste(result$log, collapse = "\n"))
  x <- result$estimates[match(app$Model, result$estimates$Model), ]
  expect_identical(x$status, app$status)
  ok <- app$status == "ok"
  for (field in c("es", "LCL", "UCL", "fit_es", "k")) expect_equal(x[[field]][ok], app[[field]][ok], tolerance = 1e-8)
}

test_that("exported code reproduces filtered, alternative, uploaded and empty selections", {
  skip_on_cran()
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  models <- file.path(root, "author-models.R")
  writeLines(c("# Author helper and constant, as models.R allows.",
    "fixed_level <- 95",
    "fixed_effect_fit <- function(df) metafor::rma(df$metaUI__effect_size, df$metaUI__variance, method = \"FE\", level = fixed_level)",
    "models_to_run <- tibble::tribble(",
    "  ~name, ~aggregated, ~es, ~LCL, ~UCL, ~k, ~code,",
    "  \"Random-Effects Multilevel Model\", FALSE, \"mod$b\", \"mod$ci.lb\", \"mod$ci.ub\", \"mod$k\", \"metaUI_multilevel_fit(df)\",",
    "  \"P-uniform star\", TRUE, \"mod$est\", \"mod$ci.lb\", \"mod$ci.ub\", \"mod$k\",",
    "    'puniform::puni_star(yi = df$metaUI__effect_size, vi = df$metaUI__variance, alpha = .05, side = \"right\", method = \"ML\", boot = FALSE)',",
    "  \"Fixed effect (author helper)\", TRUE, \"mod$b\", \"mod$ci.lb\", \"mod$ci.ub\", \"mod$k\", \"fixed_effect_fit(df)\",",
    "  # The intercept is the first factor level's mean, so it depends on the level order.",
    "  \"Reference group (author)\", FALSE, \"mod$b[1]\", \"mod$ci.lb[1]\", \"mod$ci.ub[1]\", \"mod$k\",",
    "    \"metafor::rma(df$metaUI__effect_size, df$metaUI__variance, mods = ~ factor(df$metaUI__filter_Group))\")"), models)
  x <- fixture(); x$yi <- -x$yi; x$alt <- x$yi * .9
  # Non-alphabetical levels and a trailing space must survive the workbook.
  x$year <- c(NA, 2002:2016); x$group <- factor(rep(c("G1", "G2", "G3", "G4 "), 4), levels = c("G3", "G1", "G2", "G4 "))
  d <- prepared(x, filters = c(Year = "year", Group = "group"), direction = "negative",
    alternatives = list(Corrected = list(es_field = "alt", variance = "vi", es_type = "SMD", justification = "Illustrative correction")))
  app_dir <- file.path(root, "app")
  generate_shiny(d, "Export: fixture", models = models, primary_model = "Fixed effect (author helper)",
    save_to_folder = app_dir, launch_app = FALSE)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(app_dir)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  runs <- file.path(root, c("filtered", "uploaded", "empty")); for (r in runs) dir.create(r)
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-1.8, 2.2), metaUI__filter_Year = c(2003, 2014),
      metaUI__filter_Year_include_NA = FALSE, metaUI__filter_Group = c("G1", "G3", "G4 "),
      computation = "alt1", sesoi = .3, go = 1)
    app <- estimatesfiltered()
    expect_equal(nrow(df_filtered()), 9)
    expect_identical(app$status, c("ok", "ok", "ok", "ok"))
    expect_true(app$reflected[2])
    script <- copy_download(output$executeCodeDownload, runs[1], "selection.R")
    code <- paste(readLines(script), collapse = "\n")
    expect_false(grepl("metaUI::|library\\(metaUI\\)", code))
    expect_match(code, "fixed_effect_fit <- function", fixed = TRUE)  # models.R copied verbatim
    expect_match(code, "metaUI__alt_1_fit_effect", fixed = TRUE)
    expect_match(code, "include_missing = FALSE", fixed = TRUE)
    expect_match(code, "bound <- 0.3", fixed = TRUE)
    # The optional groundhog call is commented out and parses once uncommented.
    groundhog <- grep("^# groundhog::groundhog.library", readLines(script), value = TRUE)
    expect_length(groundhog, 1L)
    call <- parse(text = sub("^# ", "", groundhog))[[1]]
    # Packages attached or used by the copied helpers count too, not only models.R.
    expect_true(all(c("metafor", "dplyr") %in% eval(call[[2]])))
    expect_match(code, "\n  dplyr = \"", fixed = TRUE)
    expect_match(call[[3]], "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
    result <- run_export(runs[1], script, output$executeDownload)
    expect_equal(result$selected, 9)
    expect_reproduces(result, app)
    expect_identical(result$estimates$Model[1], "Fixed effect (author helper)")
    expect_identical(result$estimates$role[1], "primary")

    uploaded <- d; uploaded$metaUI__effect_size[3] <- -.93; uploaded$metaUI__input_effect[3] <- -.93
    state_values$uploaded_data <- uploaded
    state_values$upload_info <- list(file = "edited.xlsx", rows = 16, z_rule = "test standardisation", checkbox_note = "")
    session$setInputs(computation = "primary", go = 2)
    app <- estimatesfiltered()
    script <- copy_download(output$executeCodeDownload, runs[2], "selection.R")
    expect_match(paste(readLines(script), collapse = "\n"), "uploaded data (edited.xlsx, 16 rows)", fixed = TRUE)
    result <- run_export(runs[2], script, output$executeDownload)
    expect_reproduces(result, app)
    expect_true(-.93 %in% df_filtered()$metaUI__effect_size)

    session$setInputs(outliers_z_scores = c(5, 6), go = 3)
    expect_equal(nrow(df_filtered()), 0)
    result <- run_export(runs[3], copy_download(output$executeCodeDownload, runs[3], "selection.R"), output$executeDownload)
    expect_null(result$estimates)
    expect_equal(result$selected, 0)
    expect_true(result$agreement$agrees)
  })
})

test_that("exported Quarto file has the same analysis and renders to the same rows", {
  skip_on_cran()
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  x <- fixture(); x$r <- x$yi / 2; x$vr <- x$vi / 4
  x$z <- atanh(x$r); x$vz <- x$vr / (1 - x$r^2)^2
  d <- prepare_data(x, "study", "r", variance = "vr", es_id = "id", es_type = "COR", variance_scale = "r",
    alternatives = list(Z = list(es_field = "z", variance = "vz", es_type = "ZCOR", variance_scale = "z", justification = "Same information on Fisher z")))
  bayes <- if (requireNamespace("bayesmeta", quietly = TRUE)) list(enabled = TRUE) else NULL
  app_dir <- file.path(root, "app")
  generate_shiny(d, "Export correlations", models = get_model_tibble()[c(1, 2, 6), ], bayesian = bayes,
    save_to_folder = app_dir, launch_app = FALSE)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(app_dir)
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  run <- file.path(root, "run"); dir.create(run)
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-1.8, 2.2), computation = "primary", go = 1)
    app <- estimatesfiltered()
    expect_true(all(app$status == "ok"))
    workbook <- copy_download(output$executeDownload, run, "analysis-metaUIdata.xlsx")
    qmd <- copy_download(output$executeQuartoDownload, run, "selection.qmd")
    lines <- readLines(qmd)
    expect_identical(lines[1], "---")
    expect_true(any(grepl("^```\\{r fit\\}$", lines)))
    expect_true(any(lines == "#| code-fold: true"))
    expect_true(any(grepl("atanh(y)", lines, fixed = TRUE)))  # primary COR computation
    if (!is.null(bayes)) expect_true(any(grepl("metaUI_bayesian_fit <- function", lines, fixed = TRUE)))
    purled <- file.path(run, "purled.R")
    knitr::purl(qmd, purled, quiet = TRUE)
    result <- run_export(run, purled, workbook)
    expect_reproduces(result, app)
    expect_equal(result$estimates$es, tanh(result$estimates$fit_es))
  })
  skip_if(!nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  setwd(run)
  status <- system2("quarto", c("render", "selection.qmd", "--quiet"), stdout = TRUE, stderr = TRUE)
  html <- paste(readLines(file.path(run, "selection.html"), warn = FALSE), collapse = "\n")
  expect_match(html, "Agrees with the app")
})

test_that("Dannheim export reproduces the source reference and a filtered selection", {
  skip_on_cran()
  source <- system.file("examples", "dannheim", package = "metaUI")
  root <- tempfile(); dir.create(root); on.exit(unlink(root, recursive = TRUE))
  file.copy(list.files(source, full.names = TRUE), root)
  wd <- getwd(); on.exit(setwd(wd), add = TRUE); setwd(root)
  sys.source("build.R", new.env(parent = globalenv()))
  setwd(file.path(root, "mental-health-app"))
  env <- new.env(parent = globalenv()); sys.source("global.R", env)
  x <- read.csv(file.path(root, "mental-health.csv"))
  built <- readRDS("dataset.rds")
  runs <- file.path(root, c("all", "filtered")); for (r in runs) dir.create(r)
  primary <- "Source-reference: REML / Hartung-Knapp (rounded inputs)"
  keep <- built$metaUI__es_z >= -1 & built$metaUI__es_z <= 1
  reference <- metafor::rma(yi = built$metaUI__effect_size[keep], sei = built$metaUI__se[keep], method = "REML", test = "knha")
  shiny::testServer(env$server, {
    session$setInputs(outliers_z_scores = c(-10, 10), go = 1)
    result <- run_export(runs[1], copy_download(output$executeCodeDownload, runs[1], "selection.R"), output$executeDownload)
    expect_reproduces(result, estimatesfiltered())
    row <- result$estimates[result$estimates$Model == primary, ]
    expect_identical(result$estimates$role[1], "primary")
    expect_equal(c(row$es, row$LCL, row$UCL), c(-0.38356080, -0.68781519, -0.07930642), tolerance = 1e-7)
    expect_true(any(grepl("Primary analysis (Source-reference: REML / Hartung-Knapp (rounded inputs)): -0.3836 [-0.6878, -0.0793], k = 12",
      result$log, fixed = TRUE)))

    session$setInputs(outliers_z_scores = c(-1, 1), go = 2)
    expect_true(sum(keep) < 12 && sum(keep) > 2)
    result <- run_export(runs[2], copy_download(output$executeCodeDownload, runs[2], "selection.R"), output$executeDownload)
    expect_reproduces(result, estimatesfiltered())
    row <- result$estimates[result$estimates$Model == primary, ]
    expect_equal(c(row$es, row$LCL, row$UCL), c(as.numeric(reference$b), reference$ci.lb, reference$ci.ub), tolerance = 1e-8)
    expect_equal(result$selected, sum(keep))
  })
})

test_that("code helpers write exact numbers and ignore packages named in comments", {
  x <- c(.6, -0.38356079413915367, 1/3, NA, 2e-20, 12)
  expect_identical(eval(parse(text = metaUI:::metaUI_code_number(x))), x)
  expect_identical(metaUI:::metaUI_code_number(.6), "0.6")
  expect_identical(metaUI:::metaUI_code_number(numeric()), "numeric()")
  code <- c("# see metaUI::prepare_data()\nlibrary(dplyr)\nfit <- metafor::rma(1)\nlabel <- \"robumeta::robu\"",
    "meta::trimfill(meta::metagen(TE = 1))")
  expect_identical(metaUI:::metaUI_code_packages(code), c("dplyr", "meta", "metafor"))
  expect_error(metaUI:::metaUI_code_packages("if (TRUE) {"))
})
