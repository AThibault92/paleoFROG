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


  observeEvent(input$data_input,{
    data <- readr::read_csv(input$data_input$datapath)

    if (sum(colnames(data) == colnames(paleoFROG::exemple_dataset)) == 14){
      r$raw_data <- data
      select_num <- dplyr::select(r$raw_data, i_a:iii_b_p)
      gdgt_sum <- rowSums(select_num, na.rm = TRUE)

      r$data <- r$raw_data %>%
        dplyr::select(ID, !!colnames(select_num)) %>%
        dplyr::mutate(row_sum = gdgt_sum) %>%
        tidyr::pivot_longer(cols = -c(ID, row_sum)) %>%
        dplyr::mutate(
          new_value = value/row_sum
        ) %>%
        tidyr::pivot_wider(names_from = name, values_from = new_value, id_cols = ID)
      shinyalert::shinyalert(title = "Success !", text = "File import done !", type = "success")
    } else {
      shinyalert::shinyalert(title = "Error !", text = "Please use the template file", type = "error")
    }
  })

  output$template <- downloadHandler(
    filename = "paleofrog_template.csv",
    content = function(file){
      readr::write_csv(paleoFROG::exemple_dataset, file)
    }
  )


  output$tab <- DT::renderDataTable({
    req(r$raw_data)
    # if (!is.na(r$raw_data)){
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
          row_sum != 1 ~ "Relative abundance was not 1, it is corrected."
        )
      ) %>%
      DT::datatable(
        class = 'cell-border stripe',
        rownames = FALSE,
        colnames = c("Sample ID", "Sum of relative abundances", "Import information"),
        selection = "none",
        options = list(
          pageLength = 20,
          searching = FALSE,
          lengthChange = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = 0:2))
        )
      ) %>%
      DT::formatStyle(
        "information",
        target = "row",
        backgroundColor = DT::styleEqual(c("No data.", "Import OK.", "Relative abundance was not 1, it is corrected."),
                                         c("#f67280", "#99ddcc","#ffffd2"))
      ) %>%
      DT::formatStyle(
        "ID",
        fontWeight = "bold"
      )
    # }

  })



}

## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")

## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1", r)

