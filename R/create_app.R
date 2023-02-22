#' Create the about text for the app
#'
#' @param dataset_name Name of the dataset
#' @param date Date of the last update. Defaults to the current date.
#' @param citation Citation requested by the author. Defaults to an empty string.
#' @param osf_link Link to the OSF (or similar) project where data and materials are available. Defaults to an empty string.
#' @param contact Contact information, typically email. Defaults to an empty string.
#' @export


create_about <- function(dataset_name, date = format(Sys.Date(), "%d %b %Y"), citation = "", osf_link = "", contact = "", ...) {

  HTML(glue::glue("<h3>Interactive multiverse meta-analysis of {dataset_name} </h3>
          <br/><br/><b>Last Update:</b> {date}
          {if (citation != '') glue::glue('<br/><br/><b>Citation:</b> {citation}') else ''}
          {if (osf_link != '') glue::glue('<br/><br/><b>Data and materials:</b> <a href={osf_link}>{osf_link}</a>') else ''}
          {if (contact != '') glue::glue('<br/><br/><b><b>Contact:</b> Contact us with suggestions or bug reports:</b> {contact}') else ''}
          <br/><br/><br/><br/> <b>Created with <a href='https://github.com/LukasWallrich/metaUI'> metaUI </a> </b>
          "))
}

#' Launch Shiny app
#'
#' @param dataset Dataframe as returned from [import_data()]
#' @param eff_size_type_label The label to be used to describe the effect size. If NA, effect type code from dataset is used.
#' @param models A tibble with the models to be included in the app. If you want to change the default models, have a look at the vignette and/or the [get_model_tibble()] documentation.
#' @inheritParams create_about
#' @inheritDotParams create_about
#' @export

launch_shiny <- function(dataset, dataset_name, eff_size_type_label = NA, models = get_model_tibble(), ...) {
  about <- create_about(dataset_name, ...)
  metaUI__df <<- dataset

  if (is.na(eff_size_type_label)) {
    eff_size_type_label <- dataset$metaUI__es_type[1]
  }

  metaUI_eff_size_type_label <<- eff_size_type_label
  models_to_run <<- models

  ui <- eval(parse(text = generate_ui(dataset, dataset_name, about)))
  server <- eval(parse(text = generate_server()))

  shinyApp(ui = ui, server = server)

}
