#' Build an app from a declarative JSON configuration
#'
#' Relative data and output paths resolve against the configuration file. Building
#' never launches or deploys an app. The configuration is deliberately limited to
#' data mapping, scientific scale/direction, an optional declared primary model,
#' metadata, presentation options, and optional study details (schema version 2).
#' `study_info` is either the path to a CSV of study details or a lookup object,
#' `{"cache": "study-metadata.csv", "abstracts": "open_licence", "openalex": true}`,
#' which runs [fetch_study_metadata()] on the mapped `url` column and writes its result
#' to the cache CSV. Only the cache is required. The lookup is the only network access
#' during a build; DOIs already found in the cache are not queried again. The generated
#' app never queries external services.
#' @param config Path to JSON, or a named list with the same schema.
#' @return Invisibly, the generated app directory.
#' @export
build_app <- function(config) {
  base <- getwd()
  if (is.character(config) && length(config) == 1L) {
    base <- dirname(normalizePath(config, mustWork = TRUE))
    config <- jsonlite::fromJSON(config, simplifyVector = FALSE)
  }
  if (!is.list(config) || is.null(names(config))) stop("Configuration must be a named list or JSON file.")
  allowed <- c("schema_version", "data", "mapping", "dataset_name", "output", "primary_model", "options", "about", "bayesian", "design", "study_info")
  unknown <- setdiff(names(config), allowed)
  if (length(unknown)) stop("Unknown configuration fields: ", paste(unknown, collapse = ", "))
  if (length(config$schema_version) != 1L || !is.numeric(config$schema_version) || !config$schema_version %in% c(1, 2)) stop("schema_version must be 1 or 2")
  if (config$schema_version == 1 && (any(c("bayesian", "study_info") %in% names(config)) || any(c("alternatives", "primary_label") %in% names(config$mapping)))) stop("New computation/Bayesian/study_info fields require schema_version 2")
  for (key in c("data", "dataset_name", "output"))
    if (!is.character(config[[key]]) || length(config[[key]]) != 1L || !nzchar(config[[key]])) stop("Required scalar string: ", key)
  info <- config$study_info
  scalar <- function(x, check) length(x) == 1L && check(x) && !is.na(x)
  if (!is.null(info) && !(scalar(info, is.character) && nzchar(info))) {
    if (!is.list(info) || is.null(names(info)) || length(setdiff(names(info), c("cache", "abstracts", "openalex"))) ||
        !scalar(info$cache, is.character) || !nzchar(info$cache) ||
        (!is.null(info$abstracts) && !(scalar(info$abstracts, is.character) && info$abstracts %in% c("open_licence", "exclude_nc", "none", "all"))) ||
        (!is.null(info$openalex) && !scalar(info$openalex, is.logical)))
      stop("study_info must be a CSV path or a lookup object {\"cache\": path, \"abstracts\": \"open_licence\"|\"exclude_nc\"|\"none\"|\"all\", \"openalex\": true|false}")
  }
  resolve <- function(path) if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(base, path)
  mapping <- config$mapping
  if (!is.list(mapping) || is.null(names(mapping))) stop("mapping must be an object")
  allowed_mapping <- setdiff(names(formals(prepare_data)), "data")
  if (length(setdiff(names(mapping), allowed_mapping))) stop("Unknown mapping fields")
  if (!all(c("study_label", "es_field", "es_type") %in% names(mapping))) stop("Mapping requires study_label, es_field and es_type")
  if (!is.null(mapping$filters)) mapping$filters <- unlist(mapping$filters)
  if (!is.null(mapping$categorical_filters)) mapping$categorical_filters <- unlist(mapping$categorical_filters)
  dataset <- do.call(prepare_data, c(list(data = resolve(config$data)), mapping))
  opts <- config$options
  if (!is.null(opts) && (!is.list(opts) || (length(opts) && (is.null(names(opts)) || any(!nzchar(names(opts))))) || length(setdiff(names(opts), c("correlation_dependent", "max_forest_plot_rows", "selection_list_threshold", "shiny_theme", "fit_cache_entries"))))) stop("Invalid options fields")
  if (!is.null(config$primary_model) && (!is.character(config$primary_model) || length(config$primary_model) != 1L))
    stop("primary_model must be one model name")
  about <- config$about
  if (!is.null(about) && (!is.list(about) || (length(about) && (is.null(names(about)) || any(!nzchar(names(about))))) || length(setdiff(names(about), c("date", "citation", "osf_link", "contact"))))) stop("Invalid about fields")
  do.call(generate_shiny, c(list(dataset = dataset, dataset_name = config$dataset_name,
                                save_to_folder = resolve(config$output), launch_app = FALSE,
                                primary_model = config$primary_model,
                                bayesian = config$bayesian, design = config$design,
                                study_info = if (is.null(info)) NULL else if (is.character(info)) resolve(info) else
                                  fetch_study_metadata(dataset, cache = resolve(info$cache),
                                    abstracts = if (is.null(info$abstracts)) "open_licence" else info$abstracts,
                                    openalex = !isFALSE(info$openalex)), options = if (is.null(config$options)) list() else config$options), about))
}
