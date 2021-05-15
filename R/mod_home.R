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
    p("This application allows:"),
    tags$ul(
      tags$li(
        "Visualize brGDGTs relative abundance"
      ),
      tags$li(
        "Compute brGDGT indices"
      ),
      tags$li(
        HTML("Predict MAAT using MBT'<sub>5Me</sub> index and FROGs<sup>*</sup> models")
      )
    ),
    tags$i(HTML("<sup>*</sup>FROG : random <b>F</b>orest <b>R</b>egression for pale<b>O</b>MAAT using br<b>G</b>DGTs")),
    tags$br(),
    tags$br(),
    p("These models are based on the following publication:", tags$a("APPLICATION OF A MACHINE-LEARNING ALGORITHM FOR THE DEVELOPMENT OF A GLOBAL BRANCHED GDGT TEMPERATURE CALIBRATION IN TERRESTRIAL SETTINGS"))
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

