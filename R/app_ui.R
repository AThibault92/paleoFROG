#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    navbarPage(
      "PaleoFROG",
      tabPanel(
        "Home",
        mod_home_ui("home_ui_1")
      ),
      tabPanel(
        "Load dataset",
        mod_load_data_ui("load_data_ui_1")
      ),
      tabPanel(
        "GDGTs distributions",
        mod_gdgt_distribution_ui("gdgt_distribution_ui_1")
      ),
      tabPanel(
        "Indices",
        mod_indices_ui("indices_ui_1")
      ),
      tabPanel(
        "MAAT prediction",
        mod_maat_prediction_ui("maat_prediction_ui_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'paleoFROG'
    ),
    tags$style(
      HTML(".row:{
          margin-left: 0px!important;
          margin-right: 0px!important;
      }"
    )),
    shinyalert::useShinyalert()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

