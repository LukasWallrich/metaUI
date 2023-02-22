#' Create the about text for the app
#'
#' @param dataset_name Name of the dataset
#' @param date Date of the last update. Defaults to the current date.
#' @param citation Citation requested by the author. Defaults to an empty string.
#' @param osf_link Link to the OSF (or similar) project where data and materials are available. Defaults to an empty string.
#' @param contact Contact information, typically email. Defaults to an empty string.
#' @export


create_about <- function(dataset_name, date = format(Sys.Date(), "%d %b %Y"), citation = "", osf_link = "", contact = "", ...) {

  glue::glue("<h3>Interactive multiverse meta-analysis of {dataset_name} </h3>
          <br/><br/><b>Last Update:</b> {date}
          {if (citation != '') glue::glue('<br/><br/><b>Citation:</b> {citation}') else ''}
          {if (osf_link != '') glue::glue('<br/><br/><b>Data and materials:</b> <a href={osf_link}>{osf_link}</a>') else ''}
          {if (contact != '') glue::glue('<br/><br/><b><b>Contact:</b> Contact us with suggestions or bug reports:</b> {contact}') else ''}
          <br/><br/><br/><br/> <b>Created with <a href='https://github.com/LukasWallrich/metaUI'> metaUI </a> </b>
          ")
}

#' Generate Shiny app
#'
#' This function generates the Shiny app. It is the main function of the package. The app can either be launched directly, or saved to
#' a folder. If saved, that folder will contain all the code and data needed to run the app. The app can then be launched by sourcing
#' the app.R file in the folder and running launch_app().
#'
#' @param dataset Dataframe as returned from [import_data()]
#' @param eff_size_type_label The label to be used to describe the effect size. If NA, effect type code from dataset is used.
#' @param models The models to be included in the app. Can either be a function to call, a tibble, or the path to a file. If you want to change the default models, have a look at the vignette and/or the [get_model_tibble()] documentation.
#' Passing a file (e.g. "my_models.R") is particularly helpful if you include helper functions and save the app. If so,
#' this must assign the tibble to a global variable called models_to_run (i.e. using <<-).
#' @param save_to_folder If specified, the code and data for the app will be saved to this folder. Defaults to NA, which means that nothing will be saved. If the folder exists, the user will
#' be asked to confirm overwriting it - unless the script is run in non-interactive mode, in which case the folder will be overwritten without asking.
#' @param launch_app Should the app be launched? Defaults to TRUE if it is not saved (i.e. save_to_folder is NA), FALSE otherwise.
#' @inheritParams create_about
#' @inheritDotParams create_about
#' @export

generate_shiny <- function(dataset, dataset_name, eff_size_type_label = NA,
        models = get_model_tibble, save_to_folder = NA, launch_app = is.na(save_to_folder), ...) {
  about <- create_about(dataset_name, ...)

  if (!is.data.frame(dataset)) stop("Dataset must be a data.frame or tibble")

  metaUI__df <<- dataset

  if (is.na(eff_size_type_label)) {
    eff_size_type_label <- dataset$metaUI__es_type[1]
  }

  metaUI_eff_size_type_label <<- eff_size_type_label

  models_from_function <- FALSE

  if (is.character(models)) {
    source(models)
  } else if (is.function(models)) {
    models_to_run <<- models()
    models_from_function <- TRUE
  } else if (is.data.frame(models) || is_tibble(models)) {
    models_to_run <<- models
  } else {
    stop("Invalid argument type. models must be a function, a tibble, or a path to a file.")
  }

  ui <- generate_ui(dataset, dataset_name, about)
  server <- generate_server()

  if (!is.na(save_to_folder)) {
  if (dir.exists(save_to_folder)) {
    if (interactive()) {
      if (!utils::askYesNo(glue::glue("Folder {save_to_folder} already exists. Overwrite?"))) {
        stop("Folder already exists. Aborting.")
      }
    }
  } else {
    dir.create(save_to_folder)
  }

  file.copy(system.file("template_code", "helpers.R", package="metaUI"),
            file.path(save_to_folder, "helpers.R"))
  if (is.character(models)) {
    file.copy(models, file.path(save_to_folder, "models.R"))
  } else {
    writeLines(paste("models_to_run <- ", to_tribble(models_to_run)), file.path(save_to_folder, "models.R"))
    if (models_from_function) {
      warning("Models tibble was created from a function. Note that any side effects (e.g., helper functions)",
        " of that function were not be saved. If you need them, edit the models.R file manually.",
        call. = FALSE, immediate. = TRUE)
  }
  }
  writeLines(labels_and_options(for_saving = TRUE), file.path(save_to_folder, "labels_and_options.R"))
  writeLines(ui, file.path(save_to_folder, "ui.R"))
  writeLines(server, file.path(save_to_folder, "server.R"))
  writeLines(generate_app.R(), file.path(save_to_folder, "app.R"))
  saveRDS(dataset, file.path(save_to_folder, "dataset.rds"))
}

if (launch_app) {
  eval(parse(text = labels_and_options()))
  ui <- eval(parse(text = ui))
  server <- eval(parse(text = server))
  shinyApp(ui = ui, server = server)
}

}

#' Generate file to launch Shiny app
#'
#' This function creates the app.R file that will be used if the app is saved. It brings together all code files and data
#' and offers the launch_app() function to launch the app.
#'
#' @noRd

generate_app.R <- function() {
  req_packages <- pacman::p_depends(metaUI, local = TRUE) %>% purrr::pluck("Imports")
  metaUI_eff_size_type_label <- metaUI_eff_size_type_label  %>% stringr::str_replace("'", stringr::fixed("\\\\'"))

  glue::glue("

  # To launch the app, source this file and then run launch_app()

  library(dplyr)

  # Ensure required packages are installed
  {purrr::map_chr(req_packages, ~ glue::glue('
      if (!requireNamespace(\"{.x}\", quietly = TRUE)) {{
        install.packages(\"{.x}\")
        }}'))  %>% glue::glue_collapse(sep = '\n')}

  # Ensure component files can be found
   f <- ''

  if(!(file.exists('server.R'))) {{
    message('server.R not found in current working directory. Trying to automatically ',
            'identify location of this script - in case of errors, please set the ',
            'working directory.')
  }}

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
        this_file <- rstudioapi::getSourceEditorContext()$path
      }}
      return(dirname(this_file))
    }}

    f <- getCurrentFileLocation()

  # Source and set elements of app
  library(shiny)
  source(file.path(f, 'helpers.R'))
  source(file.path(f, 'models.R'))
  source(file.path(f, 'labels_and_options.R'))
  server <- source(file.path(f, 'server.R')) %>% purrr::pluck('value')
  ui <- source(file.path(f, 'ui.R'))  %>% purrr::pluck('value')
  metaUI_eff_size_type_label <- '{metaUI_eff_size_type_label}'
  metaUI__df <- readRDS(file.path(f, 'dataset.rds'))

  # Create function to launch the app

  launch_app <- function() {{
    shinyApp(ui = ui, server = server)
  }}")
}


#' Convert a tibble/dataframe to tribble code
#'
#' Tribbles are an easy way to legibly input data, and therefore helpful for teaching
#' and interactive work. This function takes
#' a tibble and returns code that can recreate it. Note that this function converts
#' "NA" to NA and converts factors to characters to retain the levels.
#'
#' Taken from timesaveR package, copyright also by Lukas Wallrich, 2023
#'
#' @param x The tibble/dataframe to be converted into tribble code
#' @param show Logical. Print code (otherwise, returned - print with `cat()` to get linebreaks etc)
#' @param digits Number of digits to round numeric columns to.
#' @examples
#' to_tribble(mtcars, show = TRUE)

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
