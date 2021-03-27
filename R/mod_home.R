#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Welcome to the paleoFROG application!"),
    p("This application allows paleoMAAT reconstruction using brGDGTs."),
    p("Lien vers la publication")
  )
}

#' home Server Function
#'
#' @noRd
mod_home_server <- function(input, output, session){
  ns <- session$ns

}

## To be copied in the UI
# mod_home_ui("home_ui_1")

## To be copied in the server
# callModule(mod_home_server, "home_ui_1")

