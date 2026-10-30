# This file is copied verbatim into generated apps. Keep it independent of metaUI.
# Writes an R script or Quarto file that reproduces one applied selection from the
# app's workbook. It copies the app's models.R and the fitting functions it needs,
# so the exported code runs the same code path as the app, without metaUI.

metaUI_code_number <- function(x) {
  x <- as.numeric(x)
  if (!length(x)) return("numeric()")
  # Shortest text that reads back as the same double.
  values <- vapply(x, function(v) if (is.na(v)) "NA" else {
    for (digits in 15:17) if (as.numeric(text <- sprintf(paste0("%.", digits, "g"), v)) == v) break
    text
  }, character(1))
  if (length(x) == 1L) values else paste0("c(", paste(values, collapse = ", "), ")")
}

metaUI_code_string <- function(x) {
  if (!length(x)) return("character()")
  paste(deparse(as.character(x), width.cutoff = 500L), collapse = "")
}

# Top-level assignments of an R file, with their source text (comments kept).
metaUI_code_definitions <- function(files) {
  defs <- list()
  for (file in files[file.exists(files)]) {
    exprs <- parse(file, keep.source = TRUE)
    refs <- attr(exprs, "srcref")
    for (i in seq_along(exprs)) {
      e <- exprs[[i]]
      if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]))
        defs[[as.character(e[[2]])]] <- list(text = paste(as.character(refs[[i]]), collapse = "\n"),
          expr = e[[3]], file = basename(file))
    }
  }
  defs
}

# Definitions reachable from `roots`, in file order.
metaUI_code_closure <- function(roots, defs, exclude = character()) {
  included <- character()
  queue <- intersect(roots, names(defs))
  while (length(queue)) {
    name <- queue[1]; queue <- queue[-1]
    if (name %in% c(included, exclude)) next
    included <- c(included, name)
    queue <- c(queue, setdiff(intersect(all.names(defs[[name]]$expr), names(defs)), included))
  }
  defs[names(defs) %in% included]
}

# Packages named in code (pkg::f or library(pkg)); comments are ignored.
metaUI_code_packages <- function(code) {
  used <- unlist(lapply(code, function(x) {
    tokens <- tryCatch(utils::getParseData(parse(text = x, keep.source = TRUE)), error = function(e) NULL)
    if (is.null(tokens)) return(character())
    calls <- which(tokens$token == "SYMBOL_FUNCTION_CALL" & tokens$text %in% c("library", "require"))
    attached <- vapply(calls, function(i) {
      after <- tokens[tokens$line1 == tokens$line1[i] & tokens$col1 > tokens$col2[i] & tokens$token %in% c("SYMBOL", "STR_CONST"), ]
      if (nrow(after)) gsub("[\"']", "", after$text[1]) else ""
    }, character(1))
    c(tokens$text[tokens$token == "SYMBOL_PACKAGE"], attached)
  }))
  base <- c("base", "stats", "utils", "methods", "graphics", "grDevices", "tools", "parallel", "grid")
  sort(setdiff(unique(used[nzchar(used)]), base))
}

# format: "R" (plain script) or "qmd" (Quarto). `applied` is the filter record saved
# with the results; `estimates` is the app's result table (NULL: no eligible rows).
metaUI_reproduction_code <- function(format = c("R", "qmd"), applied, filters, source, built,
                                     models, estimates, settings, app_dir = ".", env = parent.frame()) {
  format <- match.arg(format)
  applied <- as.data.frame(applied)
  saved <- split(applied$selection, applied$id)
  specs <- metaUI_computation_specs(built)
  scale <- built$metaUI__display_scale[1]
  section <- function(title, text = character(), code = character(), label = NULL)
    list(title = title, text = text, code = code, label = label)

  # Model specifications: the app's models.R, if it still yields the loaded table.
  models_file <- file.path(app_dir, "models.R")
  models_text <- if (file.exists(models_file)) paste(readLines(models_file, warn = FALSE), collapse = "\n") else ""
  unchanged <- nzchar(models_text) && isTRUE(tryCatch({
    check <- new.env(parent = env)
    eval(parse(text = models_text), check)
    identical(check$models_to_run, models)
  }, error = function(e) FALSE))
  models_note <- if (unchanged) "Copied unchanged from the app's models.R." else
    "The app's models.R could not be read or no longer matches the models the app loaded, so this table is the loaded specification; helper code from models.R is not included."
  if (!unchanged) models_text <- paste0("models_to_run <- ", paste(deparse(as.data.frame(models), width.cutoff = 500L), collapse = "\n"))

  # Fitting functions reachable from the engine, the model code and models.R.
  files <- file.path(app_dir, c("helpers.R", "analysis.R", "computations.R", "bayesian.R", "dmetar_contributions.R"))
  defs <- metaUI_code_definitions(files)
  model_code <- unlist(models[c("code", "es", "LCL", "UCL", "k")], use.names = FALSE)
  model_code <- c(model_code[!is.na(model_code) & model_code != "NA"], models_text)
  roots <- c("metaUI_fit_models", "metaUI_interval_assessment",
    unlist(lapply(model_code, function(x) tryCatch(all.names(parse(text = x)), error = function(e) character()))))
  model_defs <- tryCatch(names(metaUI_code_definitions({
    tmp <- tempfile(fileext = ".R"); writeLines(models_text, tmp); tmp
  })), error = function(e) character())
  helpers <- metaUI_code_closure(roots, defs, exclude = model_defs)
  helper_files <- unique(vapply(helpers, function(x) x$file, character(1)))
  pcurve_note <- if ("pcurve" %in% names(helpers))
    " pcurve() is adapted from dmetar (Mathias Harrer, 2020, MIT licence; see COPYRIGHTS in the app folder)." else ""

  upload <- settings$upload
  custom <- setdiff(models$name, c(settings$known_models, models$name[grepl("^metaUI_bayesian_fit\\(", models$code)]))
  data_kind <- if (is.null(upload)) "the authors' built dataset" else
    paste0("uploaded data (", upload$file, ", ", upload$rows, " rows), not the authors' built dataset")
  intro <- c(
    paste0("This file reproduces one selection from the metaUI app \"", gsub("[\r\n]+", " ", settings$dataset_name),
      "\": its filters, missing-value choices, effect-size computation and equivalence bound, and the model rows on the Summary tab. ",
      "It was exported on ", settings$exported, " from the selection behind the results then shown (the last Analyze data)."),
    paste0("It reads the workbook downloaded from the same analysis (", settings$workbook, "), which holds every row of ", data_kind, ". ",
      "Put the workbook next to this file or set `workbook` below. The code does not need metaUI: the model specifications are the app's models.R, ",
      "and the fitting functions are copied from the app's ", paste(helper_files, collapse = ", "), "."),
    if (!is.null(upload)) paste0("For uploaded data the app recomputes descriptive z-scores against the built data (", upload$z_rule,
      "); the workbook contains these values."),
    paste0("Not reproduced: the plots and diagnostic tabs (forest plot, funnel plot and Egger's test, p-curve, z-curve, outlier diagnostics, ",
      "heterogeneity, moderation and sample breakdown), the effect-computation comparison and the Bayesian prior sensitivity. ",
      "The workbook records some of these results."),
    paste0(if (length(custom)) paste0("Rows defined by the authors rather than metaUI's defaults (", paste(custom, collapse = "; "),
      ") run the authors' code; if it uses random numbers or external state, its results can differ from the app's. ") else "",
      "The app may reuse an identical earlier fit from the same session; cached values come from the same inputs. ",
      "Other package versions can also change results. The last section compares every row with the values the app reported."))

  versions_placeholder <- "<<versions>>"
  packages <- section("Packages",
    paste0("The versions are those the app used when it produced these results (", R.version.string, "). ",
      "The app attaches dplyr, which models.R may use."),
    c(versions_placeholder,
      "missing <- names(app_versions)[!vapply(names(app_versions), requireNamespace, logical(1), quietly = TRUE)]",
      "if (length(missing)) stop(\"Install the missing packages first: install.packages(c(\", paste0('\"', missing, '\"', collapse = \", \"), \"))\")",
      "installed <- vapply(names(app_versions), function(p) as.character(utils::packageVersion(p)), character(1))",
      "if (any(installed != app_versions)) {",
      "  message(\"Installed package versions differ from the app's:\")",
      "  print(data.frame(app = app_versions, installed = installed)[installed != app_versions, ])",
      "}",
      "suppressPackageStartupMessages(library(dplyr))",
      if (format == "qmd") "options(width = 160)  # keep result tables on one line in the rendered page"), "packages")

  helper_section <- section("Fitting functions",
    paste0("Copied from the app's ", paste(helper_files, collapse = ", "), ". `metaUI_fit_models()` is the function the app calls. ",
      "For each model it checks the inputs the estimator needs, fits it, flips the effects for right-sided selection models when the declared ",
      "direction is negative, extracts the estimate, interval and k, and back-transforms correlations to r.", pcurve_note),
    vapply(helpers, function(x) x$text, character(1)), "functions")

  primary_text <- if (is.null(settings$primary_model)) "The authors declared no primary model; the rows are alternative estimators." else
    paste0("The authors declared \"", settings$primary_model, "\" as their primary analysis. Other rows are explorations.")
  models_section <- section("Model specifications",
    c(paste(models_note, "Each row names a model, whether it uses study-level (aggregated) data, the code that fits it to `df`,",
      "and the expressions that extract the estimate, interval limits and k from the fitted object `mod`."), primary_text),
    c(models_text, paste0("primary_model <- ", if (is.null(settings$primary_model)) "NULL" else metaUI_code_string(settings$primary_model))),
    "models")

  columns <- names(source)
  types <- vapply(source, function(x) if (is.numeric(x)) "numeric" else if (is.logical(x)) "logical" else "text", character(1))
  numeric_filters <- vapply(Filter(function(x) x$type == "numeric", filters), function(x) x$col, character(1))
  data_code <- c(
    paste0("workbook <- ", metaUI_code_string(settings$workbook)),
    if (format == "R") c("arguments <- commandArgs(trailingOnly = TRUE)", "if (length(arguments)) workbook <- arguments[1]"),
    "if (!file.exists(workbook)) {",
    "  candidates <- list.files(pattern = \"metaUIdata\\\\.xlsx$\")",
    "  if (length(candidates) != 1L) stop(\"Workbook not found. Put \", workbook, \" next to this file or set `workbook` to its path.\")",
    "  message(\"Using workbook \", candidates)",
    "  workbook <- candidates",
    "}",
    paste0("columns <- ", metaUI_code_string(columns)),
    paste0("column_types <- ", metaUI_code_string(unname(types))),
    "if (!identical(names(readxl::read_xlsx(workbook, sheet = \"dataset\", n_max = 0)), columns))",
    "  stop(\"The workbook's dataset sheet does not have the app's columns.\")",
    "data <- as.data.frame(readxl::read_xlsx(workbook, sheet = \"dataset\", col_types = column_types))",
    paste0("if (nrow(data) != ", nrow(source), ") warning(\"The workbook has \", nrow(data), \" rows; the app's data had ", nrow(source), ".\")"),
    "# The filters sheet records the selection behind the workbook's results.",
    "saved_filters <- as.data.frame(readxl::read_xlsx(workbook, sheet = \"filters\", col_types = \"text\"))",
    paste0("applied_filters <- data.frame(id = ", metaUI_code_string(applied$id), ",\n  selection = ",
      metaUI_code_string(applied$selection), ")"),
    "if (!identical(saved_filters$id, applied_filters$id) || !identical(saved_filters$selection, applied_filters$selection))",
    "  warning(\"The workbook's saved filters differ from this file's selection; it may come from another analysis.\")")
  data_section <- section("Data",
    paste("The column types are those of the app's data, so identifiers keep their exact text.",
      "The app's z-scores are descriptive, standardised against the built data."), data_code, "data")

  # Applied selection, in the app's order: filters, z-score range, computation.
  label <- function(col) sub("^metaUI__filter_", "", col)
  filter_code <- unlist(lapply(filters, function(f) {
    value <- saved[[f$id]]
    if (f$type == "numeric") {
      include <- !identical(toupper(saved[[paste0(f$id, "_include_NA")]][1]), "FALSE")
      c(paste0("# ", label(f$col), ": ", value[1], " to ", value[2], "; missing values ", if (include) "included" else "excluded"),
        paste0("selected <- selected[keep_range(selected[[", metaUI_code_string(f$col), "]], ", metaUI_code_number(as.numeric(value[1:2])),
          ", include_missing = ", include, "), , drop = FALSE]"))
    } else {
      value <- value[!is.na(value)]
      c(paste0("# ", label(f$col), ": ", if (length(value)) paste(length(value), "categories") else "no categories selected"),
        paste0("selected <- selected[selected[[", metaUI_code_string(f$col), "]] %in% ", metaUI_code_string(value), ", , drop = FALSE]"))
    }
  }))
  z <- as.numeric(saved$outliers_z_scores[1:2])
  computation <- if (length(specs) && !is.null(saved$computation)) saved$computation[1] else "primary"
  choices <- metaUI_computations(built)
  computation_name <- names(choices)[match(computation, choices)]
  computation_code <- if (!length(specs)) character() else if (computation == "primary") {
    input_scale <- attr(built, "metaUI_input_scale")
    if (is.null(input_scale)) input_scale <- attr(built, "metaUI_validation")$input_scale
    c(paste0("# ", computation_name, " (the original computation)"),
      "y <- selected$metaUI__input_effect; v <- selected$metaUI__input_variance",
      if (identical(input_scale, "COR")) "v <- v / (1 - y^2)^2; y <- atanh(y)  # r to Fisher z, delta-method variance",
      "selected$metaUI__effect_size <- y; selected$metaUI__variance <- v; selected$metaUI__se <- sqrt(v)")
  } else {
    spec <- Filter(function(x) x$id == computation, specs)[[1]]
    c(paste0("# ", spec$name, ": ", gsub("[\r\n]+", " ", spec$justification)),
      paste0("selected$metaUI__effect_size <- selected[[", metaUI_code_string(spec$fit_effect), "]]"),
      paste0("selected$metaUI__variance <- selected[[", metaUI_code_string(spec$fit_variance), "]]"),
      "selected$metaUI__se <- sqrt(selected$metaUI__variance)")
  }
  selection_section <- section("Applied selection",
    c(paste("The filters run in the app's order, followed by the z-score range",
      if (length(specs)) "and the effect-size computation." else "(the app defines no alternative effect-size computations).",
      "Edit the values to explore another selection; the comparison at the end then reports differences from the app.")),
    c("keep_range <- function(value, range, include_missing)",
      "  (!is.na(value) & value >= range[1] & value <= range[2]) | (is.na(value) & include_missing)",
      if (length(numeric_filters)) paste0("for (column in ", metaUI_code_string(numeric_filters), ") data[[column]] <- as.numeric(data[[column]])"),
      "selected <- data", filter_code,
      "metaUI_validate_prepared(selected)",
      paste0("# Descriptive z-score range: ", z[1], " to ", z[2]),
      paste0("selected <- selected[selected$metaUI__es_z >= ", metaUI_code_number(z[1]), " & selected$metaUI__es_z <= ", metaUI_code_number(z[2]), ", ]"),
      computation_code,
      paste0("message(\"Selected \", nrow(selected), \" of \", nrow(data), \" effects; the app selected ", settings$selected_rows, " of ", nrow(source), ".\")"),
      paste0("if (nrow(selected) != ", settings$selected_rows, ") warning(\"The selection differs from the app's.\")")),
    "selection")

  aggregation_text <- if (settings$aggregation == "first") "Study-level models use the first selected effect from each study." else
    paste0("Study-level models combine the effects within each study by a GLS average that assumes a correlation of ", settings$correlation, " between them.")
  fit_section <- section("Fit the models",
    c(paste(aggregation_text, "Effect-level models use every selected effect.",
      if (scale == "r") "Correlations are fitted as Fisher z; `es`, `LCL` and `UCL` are back-transformed to r, and `fit_*` columns keep the fitting scale." else
        "Estimates are on the fitting scale (SMD)."),
      primary_text),
    c(paste0("correlation_dependent <- ", metaUI_code_number(settings$correlation)),
      paste0("aggregation_method <- ", metaUI_code_string(settings$aggregation)),
      "if (nrow(selected)) {",
      "  estimates <- metaUI_fit_models(selected, models_to_run, correlation_dependent, aggregation_method)$table",
      "  estimates$role <- if (is.null(primary_model)) \"estimator\" else ifelse(estimates$Model == primary_model, \"primary\", \"exploration\")",
      "  estimates <- estimates[order(estimates$role != \"primary\"), , drop = FALSE]",
      "  print(estimates[c(\"Model\", \"role\", \"es\", \"LCL\", \"UCL\", \"k\", \"status\")], digits = 6, row.names = FALSE)",
      "  if (!is.null(primary_model) && estimates$role[1] == \"primary\") with(estimates[1, ], cat(\"Primary analysis (\", Model, \"): \",",
      "    if (status == \"ok\") sprintf(\"%.4f [%.4f, %.4f], k = %g\", es, LCL, UCL, k) else paste0(status, \": \", reason), \"\\n\", sep = \"\"))",
      "  notes <- estimates[nzchar(estimates$reason) | nzchar(estimates$warnings), , drop = FALSE]",
      "  if (nrow(notes)) print(notes[c(\"Model\", \"reason\", \"warnings\")], row.names = FALSE)",
      "} else {",
      "  message(\"No eligible rows remain after filtering; as in the app, no models are fitted.\")",
      "  estimates <- NULL",
      "}"), "fit")

  bound <- settings$bound
  equivalence_section <- section("Practical equivalence",
    if (is.null(bound)) "No smallest effect size of interest was chosen in the app; set `bound` to assess one." else
      paste0("The reader chose a symmetric bound of ", format(bound, digits = 15), " on the displayed (", scale,
        ") scale. The assessment checks whether each reported interval lies strictly within the bounds; it does not refit models."),
    c(paste0("bound <- ", if (is.null(bound)) "NULL" else metaUI_code_number(bound)),
      paste0("known_interval_models <- ", metaUI_code_string(settings$known_models)),
      "if (!is.null(estimates))",
      paste0("  print(metaUI_interval_assessment(estimates, bound, ", metaUI_code_string(scale), ", known_interval_models)[c(\"Model\", \"assessment\", \"threshold\", \"interval\")], digits = 6, row.names = FALSE)")),
    "equivalence")

  app <- if (is.null(estimates)) data.frame(Model = character(), status = character(), es = numeric(), LCL = numeric(), UCL = numeric(), k = numeric()) else
    as.data.frame(estimates)[c("Model", "status", "es", "LCL", "UCL", "k")]
  compare_section <- section("Compare with the app",
    "The app's values, recorded at export. A row agrees when its status matches and every value differs by at most `tolerance`.",
    c(paste0("app_estimates <- data.frame(\n  Model = ", metaUI_code_string(app$Model), ",\n  status = ", metaUI_code_string(app$status),
        ",\n  es = ", metaUI_code_number(app$es), ",\n  LCL = ", metaUI_code_number(app$LCL),
        ",\n  UCL = ", metaUI_code_number(app$UCL), ",\n  k = ", metaUI_code_number(app$k), ")"),
      "tolerance <- 1e-6",
      "same <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= tolerance)",
      "if (is.null(estimates) || !nrow(app_estimates)) {",
      "  agreement <- data.frame(Model = \"(no models fitted)\", agrees = is.null(estimates) && !nrow(app_estimates))",
      "} else {",
      "  x <- estimates[match(app_estimates$Model, estimates$Model), , drop = FALSE]",
      "  agreement <- data.frame(Model = app_estimates$Model, app_status = app_estimates$status, status = x$status,",
      "    max_difference = pmax(abs(x$es - app_estimates$es), abs(x$LCL - app_estimates$LCL), abs(x$UCL - app_estimates$UCL), abs(x$k - app_estimates$k), na.rm = TRUE),",
      "    agrees = !is.na(x$status) & x$status == app_estimates$status & same(x$es, app_estimates$es) &",
      "      same(x$LCL, app_estimates$LCL) & same(x$UCL, app_estimates$UCL) & same(x$k, app_estimates$k))",
      "}",
      "print(agreement[intersect(c(\"Model\", \"status\", \"max_difference\", \"agrees\"), names(agreement))], digits = 3, row.names = FALSE)",
      "if (all(agreement$agrees)) message(\"Agrees with the app: \", nrow(agreement), \" row(s) within tolerance \", tolerance, \".\") else",
      "  warning(\"Some results differ from the app; see the agreement table.\")"),
    "compare")

  sections <- list(packages, helper_section, models_section, data_section, selection_section, fit_section, equivalence_section, compare_section)
  code <- paste(setdiff(unlist(lapply(sections, function(s) s$code)), versions_placeholder), collapse = "\n")
  used <- metaUI_code_packages(c(code, model_code))
  versions <- vapply(used, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_), character(1))
  version_code <- paste0("app_versions <- c(", paste0("\n  ", used, " = ", vapply(versions, metaUI_code_string, character(1)), collapse = ","), ")")
  sections[[1]]$code[sections[[1]]$code == versions_placeholder] <- version_code
  title <- paste0("Reproduce a metaUI selection: ", gsub("[\r\n]+", " ", settings$dataset_name))

  if (format == "R") {
    comment <- function(text) unlist(lapply(text, function(x) c(strwrap(x, width = 78, prefix = "# "), "#")))
    out <- c(paste("#", title), "#", comment(intro), "# Licence: the fitting functions are metaUI code (GPL-3-or-later); models.R is the authors' code.", "")
    for (s in sections) out <- c(out, paste0("# ", s$title, " ", strrep("-", max(4L, 74L - nchar(s$title)))),
      if (length(s$text)) utils::head(comment(s$text), -1L), s$code, "")
  } else {
    out <- c("---", paste0("title: ", jsonlite::toJSON(title, auto_unbox = TRUE)),
      paste0("date: ", jsonlite::toJSON(settings$exported, auto_unbox = TRUE)),
      "format:", "  html:", "    embed-resources: true", "    toc: true", "execute:", "  echo: true", "---", "",
      paste0(intro, "\n"), "Licence: the fitting functions are metaUI code (GPL-3-or-later); models.R is the authors' code.", "")
    for (s in sections) out <- c(out, paste("##", s$title), "", paste0(s$text, "\n"),
      paste0("```{r ", s$label, "}"), if (identical(s$label, "functions")) c("#| code-fold: true", "#| code-summary: \"Show the fitting functions\""),
      s$code, "```", "")
  }
  paste(out, collapse = "\n")
}
