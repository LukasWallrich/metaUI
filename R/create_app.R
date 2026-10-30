declared_imports <- function() {
  imports <- utils::packageDescription("metaUI")$Imports
  if (is.null(imports) || !nzchar(imports)) stop("Cannot read metaUI's declared Imports; install metaUI before generating apps.")
  imports <- trimws(strsplit(gsub("\\([^)]*\\)", "", imports), ",")[[1]])
  imports[nzchar(imports)]
}

# Inspect trusted author code, including helpers in an author model file.
models_use_meta <- function(code) {
  any(grepl("\\bmeta\\s*::|(?:library|require|requireNamespace)\\s*\\(\\s*['\"]?meta(?:['\"]|\\s*[,\\)])", code, perl = TRUE))
}

# Used only inside generated apps and the model fits they run; listed in Suggests.
# meta is added only when author model code uses it.
metaUI_app_suggests <- c("DT", "ggplot2", "plotly", "puniform", "readxl",
  "robumeta", "shinycssloaders", "shinyjs", "weightr", "writexl", "zcurve")

app_dependencies <- function(bayesian = FALSE, meta = FALSE) {
  sort(unique(c(declared_imports(), metaUI_app_suggests, if (bayesian) "bayesmeta", if (meta) "meta")))
}

missing_app_packages <- function(bayesian = FALSE, meta = FALSE) {
  pkgs <- app_dependencies(bayesian, meta)
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (meta && !"meta" %in% missing && utils::packageVersion("meta") < "7.0.0") missing <- c(missing, "meta")
  missing
}

metaUI_app_ready <- function() !length(missing_app_packages())

check_app_packages <- function(bayesian = FALSE, meta = FALSE) {
  missing <- missing_app_packages(bayesian, meta)
  if (length(missing)) stop("Generated apps need packages that are not installed",
    if (meta) " (meta must be >= 7.0-0)", ": ",
    paste(missing, collapse = ", "), ". Install them with install.packages(c(",
    paste0('"', missing, '"', collapse = ", "), ")).", call. = FALSE)
  invisible(TRUE)
}

#' Create the about text for the app
#'
#' This function is generally only called internally by [generate_shiny()].
#'
#' @param dataset_name Name of the dataset
#' @param date Date of the last update. Defaults to the current date.
#' @param citation Citation requested by the author. Defaults to an empty string.
#' @param osf_link Link to the OSF (or similar) project where data and materials are available. Defaults to an empty string.
#' @param contact Contact information, typically email. Defaults to an empty string.
#' @param list_packages Should all packages used by the app be listed on the About page? If TRUE, it lists the packages
#' used in the default configuration. To display different packages (e.g., because you added/removed models), pass
#' a character vector with all packages to display.
#' @param bayesian Is the optional Bayesian model enabled? If so, the default list includes bayesmeta.
#' @param meta Does author model code use meta? If so, the default list includes it.
#'
#' @returns HTML code for the `About` panel of the Shiny app
#'
#' @keywords internal


create_about <- function(dataset_name, date = format(Sys.Date(), "%d %b %Y"), citation = "", osf_link = "", contact = "", list_packages = TRUE,
                         bayesian = FALSE, meta = FALSE) {
  out <- glue::glue("<h3>Interactive multiverse meta-analysis of {dataset_name}</h3>
          <p><b>Last update:</b> {date}</p>
          {if (citation != '') glue::glue('<p><b>Citation:</b> {citation}</p>') else ''}
          {if (osf_link != '') glue::glue('<p><b>Data and materials:</b> <a href=\"{osf_link}\">{osf_link}</a></p>') else ''}
          {if (contact != '') glue::glue('<p><b>Contact</b> (suggestions or bug reports): {contact}</p>') else ''}
          <p>Created with <a href='https://github.com/LukasWallrich/metaUI'>metaUI</a> v{utils::packageVersion('metaUI')}</p>
          ")

  if (list_packages == TRUE) {
    req_packages <- app_dependencies(bayesian, meta)
  } else if (is.character(list_packages)) {
    req_packages <- list_packages
    list_packages <- TRUE
  }

  if (list_packages == TRUE) {
    # generate_shiny() checks installation later and names any missing package.
    package_versions <- purrr::map_chr(req_packages, \(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed"))

    HTML(paste0(
      out, "<h4>R packages used</h4><div class='metaui-packages'>",
      purrr::map(1:3, \(i) {
        start <- (i - 1) * ceiling(length(req_packages) / 3) + 1
        end <- min(i * ceiling(length(req_packages) / 3), length(req_packages))
        glue::glue(
          "<table><tr><th>Package</th><th>Version</th></tr>\n",
          purrr::map2(req_packages[start:end], package_versions[start:end], \(p, v) glue::glue("<tr><td>{p}</td><td>{v}</td></tr>")) %>% glue::glue_collapse("\n"),
          "</table>"
        )
      }) %>% glue::glue_collapse("\n"), "</div>"
    ))
  } else {
    out
  }
}

#' Generate Shiny app
#'
#' This function generates the Shiny app. It is the main function of the package. The app can either be launched directly, or saved to
#' a folder. If saved, that folder will contain all the code and data needed to run the app. The app can then be launched using `shiny::shinyAppDir()`
#' on that folder, or modified by editing server.R, ui.R, global.R and models.R in the folder.
#'
#' @param dataset Dataframe as returned from [prepare_data()]
#' @param eff_size_type_label The label to be used to describe the effect size. If NA, effect type code from dataset is used.
#' @param models The models to be included in the app. Can either be a function to call, a tibble, or the path to a file. If you want to change the default models, have a look at the vignette and/or the [get_model_tibble()] documentation.
#' Passing a file (e.g. "my_models.R") is particularly helpful if you include helper functions and save the app. If so,
#' this must assign the tibble to a variable called models_to_run (i.e. using <-).
#' @param primary_model Optional name of the one model (a `name` in `models`) that the authors treat as their
#' primary analysis. Generated apps show it first and label the other rows as explorations. If it cannot be
#' estimated for a reader's selection, the app reports why and does not substitute another model. The default,
#' `NULL`, declares no primary model, and the app then says that it does not choose between estimators.
#' @param filter_popups Named list of expandable filter-help content. Plain text is escaped; wrap trusted HTML in [htmltools::HTML()], for example `list(Year = htmltools::HTML("<i>Note:</i> Data collection year."))`.
#' @param save_to_folder Folder for the generated code and data. Defaults to NA, which uses a temporary folder. A nonempty destination is refused unless overwrite = TRUE. Prefer a fresh destination to preserve manual edits.
#' @param static_folder Optional destination folder for a static shinylive site. Requires
#'   the suggested shinylive package. The app is generated in save_to_folder first
#'   (a temporary folder when NA), then exported with precomputed results. Must
#'   be outside save_to_folder. Static generation never launches the app.
#' @param design Optional settings from [app_design()]. Use [design_app()] to preview and export them.
#' @param bayesian NULL (default), or list(enabled = TRUE, tau_scale = .5, max_studies = 20). Adds a deterministic study-level normal-normal model with a flat prior on the mean and a half-normal heterogeneity prior. Correlation default tau_scale is .25 on Fisher z. Requires optional bayesmeta. Sensitivity analyses use half and double the scale.
#' @param study_info Optional study information shown to readers: a data frame (or CSV path) with one row per
#' study and a `study` column matching the study IDs, as returned by [fetch_study_metadata()] or written by the
#' authors. Recognised columns: `doi`, `url`, `title`, `authors` (separated by "; "), `year`, `journal`, `volume`,
#' `issue`, `pages`, `abstract` (paragraphs separated by blank lines), `abstract_licence` (licence URL),
#' `abstract_note`, `metadata_source`, `retrieved`, plus `status` and `abstract_policy` from the lookup. Studies in
#' the effect-size list then open a details row. It is stored in the app; the app never fetches metadata. Include
#' only abstracts you may republish.
#' @param overwrite Explicit opt-in to replace generated files in a nonempty destination. Unrelated files are retained.
#' @param launch_app Should the app be launched? Defaults to TRUE if save_to_folder is NA and static_folder is NULL, FALSE otherwise. Interactive auto-printing of the returned app launches it and blocks the R console until the app stops. Use launch_app = FALSE to build without launching, then run shiny::runApp() separately when ready.
#' @param options List of more detailed options to customise your app. They all have sensible defaults and are thus rarely needed.
#'   - `max_forest_plot_rows` Numeric. What is the maximum number of effects for which a forest plot should be displayed? Defaults to 200. If more effect sizes are selected, a message is shown instead.
#'   - `shiny_theme` Character. One of the shinythemes that style the app. Defaults to "yeti", see `?shinythemes::shinythemes` for all options.
#'   - `fit_cache_entries` Integer 0 to 3. Recent exact selections per reader session; 0 disables caching. Defaults to 3 for default models and 0 for custom code.
#'   - `selection_list_threshold` Numeric. From how many filter levels should a selection box be shown instead of check boxes? Defaults to 6.
#' @inheritParams create_about
#' @inheritDotParams create_about
#'
#' @details
#' Use `generate_shiny(static_folder = ...)` to generate and publish static files
#' in one step. After editing the generated app files, use [export_static()] to
#' re-export them; regenerating the app would overwrite those edits.
#'
#' @returns With static_folder supplied, invisibly returns the static site path.
#'   Otherwise, with launch_app = FALSE, invisibly returns the generated app folder path. With launch_app = TRUE, returns a Shiny app object; printing it launches the app and blocks the console until it stops.
#' @examplesIf metaUI:::metaUI_app_ready()
#' raw <- data.frame(study = letters[1:6], d = c(.1, .3, -.1, .4, .2, .5),
#'                   vi = c(.02, .03, .02, .04, .01, .05))
#' app_data <- prepare_data(raw, "study", "d", variance = "vi")
#' folder <- tempfile("metaui-example-")
#' generate_shiny(app_data, dataset_name = "Example", save_to_folder = folder,
#'                launch_app = FALSE)
#' unlink(folder, recursive = TRUE)
#' @export

generate_shiny <- function(dataset, dataset_name, eff_size_type_label = NA,
        models = get_model_tibble, primary_model = NULL, filter_popups = list(),
        save_to_folder = NA, launch_app = is.na(save_to_folder) && is.null(static_folder), ...,
        options = list(), overwrite = FALSE, bayesian = NULL, design = NULL, study_info = NULL,
        static_folder = NULL) {

  if (!is.null(static_folder)) {
    validate_static_folder(static_folder, save_to_folder)
    launch_app <- FALSE
  }

  defaults <- list(max_forest_plot_rows = 200, shiny_theme = "yeti", selection_list_threshold = 6)
  opts <- utils::modifyList(defaults, options)
  opts$design <- if (is.null(design)) NULL else validate_design(design)
  limit <- opts$max_forest_plot_rows
  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) ||
      !is.finite(limit) || limit < 1 || limit != floor(limit))
    stop("max_forest_plot_rows must be a positive finite whole number.")

   # Evaluate so that it is TRUE when save_to_folder is NA initially
   launch_app <- launch_app
   about <- create_about(dataset_name, ..., bayesian = !is.null(bayesian))

  if (is.na(save_to_folder)) {
    save_to_folder <- tempfile()
  }

  if (!is.data.frame(dataset)) stop("Dataset must be a data.frame or tibble")
  check_removed_example(dataset)
  metaUI_validate_prepared(dataset)
  if (!is.null(study_info)) study_info <- validate_study_info(study_info, dataset)

  if (is.na(eff_size_type_label)) {
    eff_size_type_label <- if (dataset$metaUI__es_type[1] == "ZCOR") "Fisher z (model summaries: r)" else dataset$metaUI__es_type[1]
  }

  models_from_function <- models

  if (is.character(models)) {
    # Mirror the generated app: caller's attached packages are visible and global.R attaches dplyr.
    model_environment <- new.env(parent = globalenv())
    model_environment$`%>%` <- dplyr::`%>%`
    sys.source(models, envir = model_environment)
    if (!exists("models_to_run", envir = model_environment, inherits = FALSE)) stop("R script passed to models argument does not create a `models_to_run` variable.")
    models_to_run <- model_environment$models_to_run
  } else if (is.function(models)) {
    models_to_run <- models()
  } else if (is.data.frame(models)) {
    models_to_run <- models
  } else {
    stop("Invalid argument type. models must be a function, a tibble, or a path to a file.")
  }

  uses_meta <- models_use_meta(c(models_to_run$code,
    if (is.character(models)) readLines(models, warn = FALSE)))
  if (uses_meta) about <- create_about(dataset_name, ..., bayesian = !is.null(bayesian), meta = TRUE)
  bayesian <- metaUI_bayesian_options(bayesian, dataset$metaUI__es_type[1])
  package_models <- identical(models_to_run, get_model_tibble())
  if (!is.null(bayesian)) {
    if (is.character(models)) stop("For Bayesian authoring, supply model specifications instead of a model file.")
    models_to_run <- dplyr::bind_rows(models_to_run, metaUI_bayesian_spec(bayesian))
  }
  opts$bayesian <- bayesian
  check_app_packages(!is.null(bayesian), uses_meta)
  if (!is.null(primary_model) && (!is.character(primary_model) || length(primary_model) != 1L ||
      sum(models_to_run$name == primary_model) != 1L))
    stop("primary_model must name exactly one model in models_to_run: ", paste(models_to_run$name, collapse = "; "))

  # Custom code can depend on external state or randomness: cache only by opt-in.
  if (is.null(options$fit_cache_entries)) opts$fit_cache_entries <- if (package_models) 3L else 0L
  metaUI_fit_cache(opts$fit_cache_entries) # validate before writing
  report <- attr(dataset, "metaUI_validation")
  if (is.null(report)) stop("Dataset needs prepare_data() validation metadata. Re-prepare legacy datasets.")
  if (report$retained_rows != nrow(dataset) || (!is.null(report$fingerprint) && !identical(report$fingerprint, metaUI_data_fingerprint(dataset))))
    stop("Validation report is stale: rows or values changed after prepare_data(). Re-run prepare_data() on the intended input.")
  correlation <- if (is.null(options$correlation_dependent)) .6 else options$correlation_dependent
  metaUI_aggregate(dataset, correlation) # check the aggregation contract before writing
  report$aggregation <- list(method = "GLS study-level average", correlation = correlation,
    assumptions = "Distinct studies are independent; multilevel V is diagonal (sampling covariance not supplied). RVE uses correlated-effects weights (rho=.8), small=FALSE. Neither N nor p is inferred by aggregation.")
  if (!is.null(bayesian)) report$bayesian <- c(bayesian, list(
    model = "Two-level normal-normal on aggregated studies", mean_prior = "Improper uniform on fitting scale",
    latency = "Default cap 20 studies. Approximate benchmark: 20 seconds at 20 studies, 97 at 50. Each Analyze blocks the server process while fitting; sensitivity/downloads can need two extra fits. Hard author override limit 50.",
    tau_prior = "Half-normal on fitting scale", interval = "95% central credible interval; posterior median",
    sensitivity = "Half and double tau_scale", aggregation = paste("GLS study-level average, within-study correlation", correlation),
    rationale = "Default scale is unit-information SD / 4: 0.5 for SMD, 0.25 for Fisher z. An author choice, examined with sensitivity analysis.",
    reference = "https://doi.org/10.1002/jrsm.1475"))
  report$performance <- list(fit_cache_entries = opts$fit_cache_entries,
    note = "Session-local exact data/model/config keys; cached warnings/failures and original fit times retained. Custom models default to no cache. Hidden outputs remain suspended by Shiny.")
  report$primary_model <- if (is.null(primary_model)) NA_character_ else primary_model
  report$model_source <- if (is.character(models)) list(type = "trusted_author_R_file",
    file = basename(models), md5 = unname(tools::md5sum(models))) else list(type = "model_specifications")
  report$models <- lapply(seq_len(nrow(models_to_run)), function(i) {
    spec <- models_to_run[i, , drop = FALSE]
    x <- if (spec$aggregated) metaUI_aggregate(dataset, correlation) else dataset
    reason <- metaUI_model_reason(spec, x)
    list(name = spec$name, aggregated = spec$aggregated, code = spec$code,
         status = if (is.null(reason)) "eligible_not_fitted" else "unsupported", reason = reason)
  })
  report$pcurve <- tryCatch({
    selected <- metaUI_pcurve_data(dataset)
    list(status = "eligible_not_fitted", selection = "first per study before direction screening",
         direction_exclusions = attr(selected, "direction_exclusions"),
         note = "Normal Wald p-values; N not required for evidential-value plot. Author must justify selection against hypotheses/designs.")
  }, error = function(e) list(status = "unsupported", reason = conditionMessage(e)))
  report$study_info <- study_info_report(study_info, dataset)
  report$zcurve <- "First effect per study; normal Wald |effect/SE|, not supplied source p-values. Independence across study IDs is an author assumption."
  about <- paste0(about, "<h4>Input and analysis contract</h4><pre>",
                  htmltools::htmlEscape(jsonlite::toJSON(report, auto_unbox = TRUE, pretty = TRUE, na = "null"), attribute = TRUE), "</pre>")
  defaults_models <- get_model_tibble()
  opts$known_interval_models <- models_to_run$name[vapply(seq_len(nrow(models_to_run)), function(i) {
    idx <- match(models_to_run$name[i], defaults_models$name)
    !is.na(idx) && identical(as.list(models_to_run[i, names(defaults_models), drop = FALSE]),
      as.list(defaults_models[idx, , drop = FALSE]))
  }, logical(1))]
  ui <- generate_ui(dataset, dataset_name, about, filter_popups, opts = opts)
  server <- generate_server(dataset, opts = opts)

  if (dir.exists(save_to_folder) && length(list.files(save_to_folder, all.files = TRUE, no.. = TRUE)) && !overwrite)
    stop("Destination is not empty. Use a fresh folder to preserve author edits, or explicitly set overwrite = TRUE.")
  if (!dir.exists(save_to_folder)) dir.create(save_to_folder, recursive = TRUE)
  if (!dir.exists(file.path(save_to_folder, "www"))) {
    dir.create(file.path(save_to_folder, "www"))
  }

  file.copy(system.file("template_code", "helpers.R", package="metaUI"),
            file.path(save_to_folder, "helpers.R"), overwrite = TRUE)
  file.copy(system.file("template_code", "favicon.svg", package="metaUI"),
            file.path(save_to_folder, "www", "favicon.svg"), overwrite = TRUE)
  for (asset in c("metaui.css", "metaui.js"))
    file.copy(system.file("template_code", asset, package="metaUI"),
              file.path(save_to_folder, "www", asset), overwrite = TRUE)
  if (!is.null(opts$design)) writeLines(design_css(opts$design), file.path(save_to_folder, "www", "app-design.css"))
  # Rebuilding without design settings must not retain a previous theme.
  if (is.null(opts$design) && file.exists(file.path(save_to_folder, "www", "app-design.css")))
    unlink(file.path(save_to_folder, "www", "app-design.css"))
  if (!is.null(opts$design)) jsonlite::write_json(opts$design, file.path(save_to_folder, "design.json"), auto_unbox = TRUE, pretty = TRUE)
  if (is.null(opts$design) && file.exists(file.path(save_to_folder, "design.json")))
    unlink(file.path(save_to_folder, "design.json"))
  if (!is.null(study_info)) saveRDS(study_info, file.path(save_to_folder, "study_info.rds"))
  if (is.null(study_info) && file.exists(file.path(save_to_folder, "study_info.rds")))
    unlink(file.path(save_to_folder, "study_info.rds"))
  file.copy(system.file("template_code", "dmetar_contributions.R", package="metaUI"),
            file.path(save_to_folder, "dmetar_contributions.R"), overwrite = TRUE)
  if (is.character(models)) {
    file.copy(models, file.path(save_to_folder, "models.R"), overwrite = overwrite)
  } else {
    writeLines(generate_models.R(models_to_run), file.path(save_to_folder, "models.R"))
    if (is.function(models_from_function)) {
      if (!(identical(models_from_function, get_model_tibble))) {
      message("Models tibble was created from a function. Note that any side effects (e.g., helper functions)",
        " of that function were not saved. If you need them, edit the models.R file manually.",
        call. = FALSE, immediate. = TRUE)
      }
    }
  }
  file.copy(system.file("template_code", "bayesian.R", package = "metaUI"),
            file.path(save_to_folder, "bayesian.R"), overwrite = TRUE)
  file.copy(system.file("template_code", "selection_links.R", package = "metaUI"),
            file.path(save_to_folder, "selection_links.R"), overwrite = TRUE)
  file.copy(system.file("template_code", "code_export.R", package = "metaUI"),
            file.path(save_to_folder, "code_export.R"), overwrite = TRUE)
  file.copy(system.file("template_code", "computations.R", package = "metaUI"),
            file.path(save_to_folder, "computations.R"), overwrite = TRUE)
  file.copy(system.file("template_code", "analysis.R", package = "metaUI"),
            file.path(save_to_folder, "analysis.R"), overwrite = TRUE)
  jsonlite::write_json(report, file.path(save_to_folder, "validation.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
  dependencies <- app_dependencies(!is.null(bayesian), uses_meta)
  versions <- data.frame(package = dependencies,
    version = vapply(dependencies, function(x) as.character(utils::packageVersion(x)), character(1)))
  utils::write.csv(versions, file.path(save_to_folder, "dependencies.csv"), row.names = FALSE)
  file.copy(system.file("COPYRIGHTS", package = "metaUI"), file.path(save_to_folder, "COPYRIGHTS"), overwrite = TRUE)
  # Could consider keeping all labels in this file - but then ui.R needs less readable glue::glue syntax
  writeLines(labels_and_options(dataset_name, correlation, primary_model), file.path(save_to_folder, "labels_and_options.R"))
  writeLines(ui, file.path(save_to_folder, "ui.R"))
  writeLines(server, file.path(save_to_folder, "server.R"))
  writeLines(generate_global.R(eff_size_type_label, bayesian, uses_meta), file.path(save_to_folder, "global.R"))
  saveRDS(dataset, file.path(save_to_folder, "dataset.rds"))

  if (!is.null(static_folder))
    return(invisible(export_static(save_to_folder, static_folder, overwrite = overwrite)))
  if (!launch_app) return(invisible(normalizePath(save_to_folder)))
  shinyAppDir(save_to_folder)
}

#' Generate global.R to prepare for launch of Shiny app
#'
#' This function creates the global.R file that will be used if the app is saved. It complements ui.R and server.R
#' to prepare the workspace before the app is launched
#'
#' @noRd

generate_global.R <- function(metaUI_eff_size_type_label, bayesian = NULL, uses_meta = FALSE) {
  req_packages <- app_dependencies(!is.null(bayesian), uses_meta)


  meta_check <- if (uses_meta) "if (utils::packageVersion('meta') < '7.0.0') stop('meta >= 7.0-0 is required; install dependencies before launch.')" else ""
  label_code <- paste(deparse(as.character(metaUI_eff_size_type_label)), collapse = "\n")
  glue::glue("

  # To launch the app manually, use shiny::shinyAppDir(YOURPATH) or the Run App button in RStudio

  library(dplyr)

  # Ensure required packages are installed
  {purrr::map_chr(req_packages, ~ glue::glue('
      if (!requireNamespace(\"{.x}\", quietly = TRUE)) {{
        stop(\"Missing dependency: {.x}. Install dependencies before launching; see dependencies.csv.\")
        }}'))  %>% glue::glue_collapse(sep = '\n')}
  {meta_check}

  # Ensure component files can be found
   f <- '.'

  if(!(file.exists('server.R'))) {{
    message('server.R not found in current working directory. Trying to automatically ',
            'identify location of this script - in case of errors, please set the ',
            'working directory.')


    # With thanks to https://stackoverflow.com/a/55322344/10581449
    getCurrentFileLocation <-  function()
    {{
      this_file <- commandArgs() %>%
        tibble::enframe(name = NULL) %>%
        tidyr::separate(col=value, into=c('key', 'value'), sep='=', fill='right') %>%
        dplyr::filter(key == '--file') %>%
        dplyr::pull(value)
      if (length(this_file)==0)
      {{
        stop('Cannot locate app folder. Set the working directory or launch with shiny::runApp(app_folder).')
      }}
      return(dirname(this_file))
    }}

    f <- getCurrentFileLocation()
  }}

  # Source and set elements of app
  library(shiny)
  source(file.path(f, 'helpers.R'), local = TRUE)
  source(file.path(f, 'analysis.R'), local = TRUE)
  source(file.path(f, 'computations.R'), local = TRUE)
  source(file.path(f, 'selection_links.R'), local = TRUE)
  source(file.path(f, 'code_export.R'), local = TRUE)
  metaUI_app_dir <- normalizePath(f) # code export copies models.R and analysis code from here
  source(file.path(f, 'bayesian.R'), local = TRUE)
  source(file.path(f, 'models.R'), local = TRUE)
  source(file.path(f, 'labels_and_options.R'), local = TRUE)
  # Optional study details written at build time (see metaUI::fetch_study_metadata()); never fetched here.
  metaUI_study_info <- if (file.exists(file.path(f, 'study_info.rds'))) readRDS(file.path(f, 'study_info.rds')) else NULL
  metaUI_study_details_html <- if (is.null(metaUI_study_info)) character() else metaUI_study_details(metaUI_study_info)
  metaUI_study_reference_text <- if (is.null(metaUI_study_info)) character() else metaUI_study_reference(metaUI_study_info)
  source(file.path(f, 'dmetar_contributions.R'), local = TRUE)
  server <- source(file.path(f, 'server.R'), local = TRUE) %>% purrr::pluck('value')
  ui <- source(file.path(f, 'ui.R'), local = TRUE)  %>% purrr::pluck('value')
  metaUI_eff_size_type_label <- {label_code}
  metaUI__df <- readRDS(file.path(f, 'dataset.rds'))
  metaUI_dataset_key <- unname(tools::md5sum(file.path(f, 'dataset.rds')))
")
}


#' Convert a tibble/dataframe to tribble code
#'
#' Tribbles are an easy way to legibly input data, and therefore helpful for teaching
#' and interactive work. This function takes
#' a tibble and returns code that can recreate it. Note that this function converts
#' "NA" to NA and converts factors to characters to retain the levels.
#'
#' @param x The tibble/dataframe to be converted into tribble code
#' @param show Logical. Print code (otherwise, returned - print with `cat()` to get linebreaks etc)
#' @param digits Number of digits to round numeric columns to.
#' @return Code to create a tibble using the `tribble()`-function
#' @source Taken from timesaveR package, copyright also by Lukas Wallrich (2023)
#' @keywords internal
#' @examples
#' metaUI:::to_tribble(mtcars, show = TRUE)

to_tribble <- function(x, show = FALSE, digits = 5) {

  escape_quotes <- function(x) {
    x <- stringr::str_replace_all(x, "'", "\\\\'")
    x <- stringr::str_replace_all(x, '"', '\\\\"')
    x
  }

  x <- x %>% dplyr::mutate_if(is.factor, as.character)
  x <- x %>% dplyr::mutate_if(is.character, function(x) paste0('"', escape_quotes(x), '"'))
  x <- x %>% dplyr::mutate_if(is.numeric, function(x) round(x, digits))

  lengths <- pmax(
    purrr::map_int(x, ~ max(nchar(.x, keepNA = FALSE))),
    purrr::map_int(names(x), ~ nchar(.x))
  )
  vars <- names(x)

  code <- "tibble::tribble(
  "
  for (i in seq_len(length(vars))) {
    code <- glue::glue("{code}~{vars[i]}, {paste0(collapse = '', '', rep(' ', lengths[i] - nchar(vars[i])))}")
  }
  for (j in seq_len(nrow(x))) {
    code <- code %>% paste0("\n  ")
    for (i in seq_len(length(vars))) {
      code <- glue::glue("{code} {x[[j, i]]}, {paste0(collapse = '', '',rep(' ',  lengths[i] - nchar(x[[j, i]], keepNA = FALSE)))}")
    }
  }

  code <- code %>% stringr::str_replace_all(stringr::fixed('"NA"'), "NA")

  code <- code %>%stringr::str_trim() %>%
    substr(1, nchar(.) - 1) %>%
    paste0("\n)\n")

  if (show) {
    cat(code)
    return(invisible(code))
  }
  code
}

#' Generate models.R file
#'
#' This function creates the models.R file that will be used if the app is saved. It splits the tibble into two
#' (in line with the internal code) to simplify editing.
#'
#' @noRd

generate_models.R <- function(models_to_run) {

  models_to_run_chr <- to_tribble(models_to_run %>% dplyr::select(-"code"))
  models_code_chr <- to_tribble(models_to_run %>% dplyr::select("name", "code"))

  glue::glue('
             models_to_run <- {models_to_run_chr}

             models_code <- {models_code_chr}

            # KEEP THIS AS THE **LAST** LINE! Helper functions etc must be added above
            models_to_run <- models_to_run %>% dplyr::left_join(models_code, by = "name")
            models_to_run
             ')

}


# Validate before writing any generated files, including destinations not yet created.
validate_static_folder <- function(static_folder, save_to_folder) {
  if (!is.character(static_folder) || length(static_folder) != 1L ||
      is.na(static_folder) || !nzchar(static_folder))
    stop("static_folder must be a nonempty scalar string.")
  if (!requireNamespace("shinylive", quietly = TRUE))
    stop('Static generation requires the shinylive package: install.packages("shinylive").')
  if (!is.na(save_to_folder) && metaUI_paths_overlap(save_to_folder, static_folder))
    stop("static_folder must be separate from save_to_folder (not equal to, inside or containing it).")
}

# Absolute path with symlinks resolved, also for paths that do not exist yet.
metaUI_canonical_path <- function(path) {
  path <- path.expand(path)
  if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = TRUE))
  parent <- dirname(path)
  if (identical(parent, path)) return(path)
  parent <- metaUI_canonical_path(parent)
  leaf <- basename(path)
  if (leaf == "..") return(dirname(parent))
  if (leaf == ".") return(parent)
  file.path(parent, leaf)
}

# TRUE if two folders are the same or one contains the other.
metaUI_paths_overlap <- function(a, b) {
  a <- sub("/+$", "", metaUI_canonical_path(a)); b <- sub("/+$", "", metaUI_canonical_path(b))
  identical(a, b) || startsWith(a, paste0(b, "/")) || startsWith(b, paste0(a, "/"))
}

#' @name export_static
#' @rdname export_static
#' @details
#' Use `generate_shiny(static_folder = ...)` for one-step generation and static
#' export. After editing the generated app files, call `export_static()` to
#' re-export the edited app; regenerating would overwrite those edits.
NULL
