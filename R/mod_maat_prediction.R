#' maat_prediction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_maat_prediction_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      h3("Predict temperature")
    ),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        "Chose the models:",
        checkboxGroupInput(inputId = ns("model_list"), label = NULL,
                           choiceNames = c("FROG5me", "FROG", "FROG500", "FROG0"),
                           choiceValues = c("MAAT_5met", "MAAT_all", "MAAT_MAP500", "Tavg0"),
                           selected = c("MAAT_5met", "MAAT_all", "MAAT_MAP500", "Tavg0")
        ),
        actionButton(inputId = ns("run"), label = "Run the models"),
        uiOutput(outputId = ns("uiout_dl_results"))
      ),
      mainPanel(
        width = 10,
        echarts4r::echarts4rOutput(outputId = ns("plot"), height = "600px")
      )
    )
  )
}

#' maat_prediction Server Function
#'
#' @noRd
mod_maat_prediction_server <- function(input, output, session, r){
  ns <- session$ns

  observeEvent(input$run,{
    r$results <- tibble::tibble(
      ID = r$data$ID
    )
    require(workflows)
    if ("MAAT_5met" %in% input$model_list){
      pred_MAAT_5met <- predict(paleoFROG::rf_maat_5methyl, r$data)
      r$results$FROG5me <- pred_MAAT_5met$.pred
    }

    if ("MAAT_all" %in% input$model_list){
      pred_MAAT_all <- predict(paleoFROG::rf_maat_all, r$data)
      r$results$FROG <- pred_MAAT_all$.pred
    }

    if ("MAAT_MAP500" %in% input$model_list){
      pred_MAAT_map500 <- predict(paleoFROG::rf_maat_map500_all, r$data)
      r$results$FROG500 <- pred_MAAT_map500$.pred
    }

    if ("Tavg0" %in% input$model_list){
      pred_Tavg0 <- predict(paleoFROG::rf_tavg0_all, r$data)
      r$results$FROG0 <- pred_Tavg0$.pred
    }

  })

  output$uiout_dl_results <- renderUI({

    if (!is.na(r$results)){
      downloadButton(outputId = ns("dl_results"), label = "Download results")
    }

  })

  output$dl_results <- downloadHandler(
    filename = "results.csv",
    content = function(file){
      readr::write_csv(r$results, file)
    }
  )

  output$plot <- echarts4r::renderEcharts4r({

    if (!is.na(r$results)){
      r$results %>%
        tidyr::pivot_longer(cols = -ID) %>%
        dplyr::group_by(name) %>%
        echarts4r::e_charts(ID) %>%
        echarts4r::e_line(value, smooth = 0) %>%
        echarts4r::e_flip_coords() %>%
        echarts4r::e_theme("macarons") %>%
        echarts4r::e_x_axis(name = "Predicted MAAT (°C)", nameLocation = "center", nameGap = 30) %>%
        echarts4r::e_toolbox() %>%
        echarts4r::e_toolbox_feature(feature = c("saveAsImage"), title = "Download") %>%
        echarts4r::e_tooltip(
          trigger = "item"
        ) %>%
        echarts4r::e_title("MAAT predictions")
    }

  })


}

## To be copied in the UI
# mod_maat_prediction_ui("maat_prediction_ui_1")

## To be copied in the server
# callModule(mod_maat_prediction_server, "maat_prediction_ui_1", r)

