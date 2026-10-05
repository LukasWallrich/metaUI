#' Build an app from a declarative JSON configuration
#'
#' Relative data and output paths resolve against the configuration file. Building
#' never launches or deploys an app. The configuration is deliberately limited to
#' data mapping, scientific scale/direction, metadata, and presentation options.
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
  allowed <- c("schema_version", "data", "mapping", "dataset_name", "output", "options", "about")
  unknown <- setdiff(names(config), allowed)
  if (length(unknown)) stop("Unknown configuration fields: ", paste(unknown, collapse = ", "))
  if (!identical(config$schema_version, 1L) && !identical(config$schema_version, 1)) stop("schema_version must be 1")
  for (key in c("data", "dataset_name", "output"))
    if (!is.character(config[[key]]) || length(config[[key]]) != 1L || !nzchar(config[[key]])) stop("Required scalar string: ", key)
  resolve <- function(path) if (grepl("^(/|[A-Za-z]:)", path)) path else file.path(base, path)
  mapping <- config$mapping
  if (!is.list(mapping) || is.null(names(mapping))) stop("mapping must be an object")
  allowed_mapping <- setdiff(names(formals(prepare_data)), "data")
  if (length(setdiff(names(mapping), allowed_mapping))) stop("Unknown mapping fields")
  if (!all(c("study_label", "es_field", "es_type") %in% names(mapping))) stop("Mapping requires study_label, es_field and es_type")
  if (!is.null(mapping$filters)) mapping$filters <- unlist(mapping$filters)
  if (!is.null(mapping$categorical_filters)) mapping$categorical_filters <- unlist(mapping$categorical_filters)
  dataset <- do.call(prepare_data, c(list(data = resolve(config$data)), mapping))
  opts <- config$options
  if (!is.null(opts) && (!is.list(opts) || length(setdiff(names(opts), c("correlation_dependent", "max_forest_plot_rows", "selection_list_threshold", "shiny_theme"))))) stop("Invalid options fields")
  about <- config$about
  if (!is.null(about) && (!is.list(about) || length(setdiff(names(about), c("date", "citation", "osf_link", "contact"))))) stop("Invalid about fields")
  do.call(generate_shiny, c(list(dataset = dataset, dataset_name = config$dataset_name,
                                save_to_folder = resolve(config$output), launch_app = FALSE,
                                options = if (is.null(config$options)) list() else config$options), about))
}
