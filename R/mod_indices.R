#' indices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_indices_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("GDGTs indices"),
    downloadButton(outputId = ns("dl"), label = "Download all indices"),
    DT::dataTableOutput(outputId = ns("tab"))
  )
}

#' indices Server Function
#'
#' @noRd
mod_indices_server <- function(input, output, session, r){
  ns <- session$ns

  observeEvent(r$data,{
    req(r$data)
    r$indices <- tibble::tibble(
      ID = r$data$ID
    )

    # if ("mbt5me" %in% input$indice){
    r$indices$mbt5me <- (r$data$i_a + r$data$i_b + r$data$i_c)/(r$data$i_a + r$data$i_b + r$data$i_c + r$data$ii_a + r$data$ii_b + r$data$ii_c + r$data$iii_a)
    # }

    # if ("ir6me" %in% input$indice){
    r$indices$ir6me <- (r$data$ii_a_p + r$data$ii_b_p + r$data$ii_c_p + r$data$iii_a_p + r$data$iii_b_p)/
      (r$data$ii_a_p + r$data$ii_b_p + r$data$ii_c_p + r$data$iii_a_p + r$data$iii_b_p + r$data$ii_a + r$data$ii_b + r$data$ii_c + r$data$iii_a + r$data$iii_b)
    # }

    # if ("ci" %in% input$indice){
    r$indices$ci <- (r$data$i_a)/(r$data$i_a + r$data$ii_a + r$data$iii_a)

  })

  output$dl <- downloadHandler(
    filename = "indices.csv",
    content = function(file){
      readr::write_csv(r$indices, file)
    }
  )


  output$tab <- DT::renderDataTable({
    req(r$indices)
    r$indices %>%
      DT::datatable(
        class = 'cell-border stripe',
        rownames = FALSE,
        colnames = c("Sample ID", "MBT'5Me", "IR6Me", "CI"),
        selection = "none",
        options = list(
          pageLength = 50,
          searching = FALSE,
          lengthChange = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = 0:3))
        )
      ) %>%
      DT::formatRound(
        2:4,
        digits = 2
      ) %>%
      DT::formatStyle(
        'mbt5me',
        background = DT::styleColorBar(c(0,1), '#f07b3f', angle = -90),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      DT::formatStyle(
        'ir6me',
        background = DT::styleColorBar(c(0,1), 'steelblue', angle = -90),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      DT::formatStyle(
        'ci',
        background = DT::styleInterval(cuts = 0.69, values = c("#f67280", "#99ddcc"))
      )

  })

}

## To be copied in the UI
# mod_indices_ui("indices_ui_1")

## To be copied in the server
# callModule(mod_indices_server, "indices_ui_1")

