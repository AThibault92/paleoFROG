#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      h3("Load data in the application"),
      sidebarLayout(
        sidebarPanel(
          fileInput(inputId = ns("data_input"), label = "Choose your file", accept = "csv"),
          downloadButton(outputId = ns("template"), label = "Download the file template"),
          actionButton(inputId = ns("load_ex"), label = "Load exemple dataset")
        ),
        mainPanel(
          DT::dataTableOutput(outputId = ns("tab"))
        )
      )

    )
  )
}

#' load_data Server Function
#'
#' @noRd
mod_load_data_server <- function(input, output, session, r){
  ns <- session$ns

  observeEvent(input$load_ex,{

    r$raw_data <- paleoFROG::exemple_dataset

    select_num <- dplyr::select(r$raw_data, i_a:iii_b_p)
    gdgt_sum <- rowSums(select_num, na.rm = TRUE)

    r$data <- r$raw_data %>%
      dplyr::mutate(row_sum = gdgt_sum) %>%
      tidyr::pivot_longer(cols = -c(ID, row_sum)) %>%
      dplyr::mutate(
        new_value = value/row_sum
      ) %>%
      tidyr::pivot_wider(names_from = name, values_from = new_value, id_cols = ID)


  })

  output$tab <- DT::renderDataTable({

    if (!is.na(r$raw_data)){
      select_num <- dplyr::select(r$raw_data, i_a:iii_b_p)
      gdgt_sum <- rowSums(select_num, na.rm = TRUE)

      r$raw_data %>%
        dplyr::mutate(
          row_sum = gdgt_sum
        ) %>%
        dplyr::select(ID, row_sum) %>%
        dplyr::mutate(
          information = dplyr::case_when(
            is.na(row_sum) ~ "No data.",
            row_sum == 1 ~ "Import OK.",
            row_sum != 1 ~ "Relative abundance is not 1, it was corrected."
          )
        ) %>%
        DT::datatable()
    }

  })



}

## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")

## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1", r)

