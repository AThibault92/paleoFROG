#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  r <- reactiveValues(
    raw_data = NA,
    data = NA,
    indices = NA,
    results = NA
  )
  # List the first level callModules here
  callModule(mod_load_data_server, "load_data_ui_1", r)
  callModule(mod_gdgt_distribution_server, "gdgt_distribution_ui_1", r)
  callModule(mod_indices_server, "indices_ui_1", r)
  callModule(mod_maat_prediction_server, "maat_prediction_ui_1", r)
}
