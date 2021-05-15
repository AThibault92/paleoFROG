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
    h3("Predict temperature"),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        "Chose the models:",
        checkboxGroupInput(inputId = ns("model_list"), label = NULL,
                           choiceNames = c("FROG5me", "FROG", "FROG500", "FROG0", "De Jonge et al. 2014", "Naafs et al. 2017"),
                           choiceValues = c("MAAT_5met", "MAAT_all", "MAAT_MAP500", "Tavg0", "De_Jonge", "Naafs"),
                           selected = c("MAAT_5met", "MAAT_all", "MAAT_MAP500", "Tavg0", "De_Jonge", "Naafs")
        ),
        actionButton(inputId = ns("run"), label = "Run the models"),
        uiOutput(outputId = ns("uiout_dl_results"))
      ),
      column(
        width = 10,
        tabsetPanel(
          # width = 10,
          tabPanel("Table", DT::dataTableOutput(outputId = ns("tab"))),
          tabPanel("Plot", echarts4r::echarts4rOutput(outputId = ns("plot"), height = "600px"))
        )
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
    req(r$data)
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

    if ("De_Jonge" %in% input$model_list){
      mbt5me <- (r$data$i_a + r$data$i_b + r$data$i_c)/(r$data$i_a + r$data$i_b + r$data$i_c + r$data$ii_a + r$data$ii_b + r$data$ii_c + r$data$iii_a)
      r$results$`De Jonge et al. 2014` <- 31.45*mbt5me - 8.57
    }

    if ("Naafs" %in% input$model_list){
      mbt5me <- (r$data$i_a + r$data$i_b + r$data$i_c)/(r$data$i_a + r$data$i_b + r$data$i_c + r$data$ii_a + r$data$ii_b + r$data$ii_c + r$data$iii_a)
      r$results$`Naafs et al. 2017` <- 39.09*mbt5me - 14.5
    }


  })

  output$uiout_dl_results <- renderUI({

    req(r$results)
    downloadButton(outputId = ns("dl_results"), label = "Download results")

  })

  output$dl_results <- downloadHandler(
    filename = "results.csv",
    content = function(file){
      readr::write_csv(r$results, file)
    }
  )

  output$plot <- echarts4r::renderEcharts4r({

    req(r$results)
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
      echarts4r::e_title("MAAT predictions") %>%
      echarts4r::e_legend(top = 30) %>%
      echarts4r::e_tooltip(
        trigger = "item",
        formatter = htmlwidgets::JS("
    function(params){
    var colorSpan = color => '<span style=\"display:inline-block;margin-right:5px;border-radius:10px;width:9px;height:9px;background-color:' + color + '\"></span>';
      return(colorSpan(params.color) + 'Model: '+params.seriesName+'<br />Sample: ' + params.value[1]+'<br />Predicted MAAT: ' + Math.round(params.value[0]*10)/10 +'°C')
    }
    ")
      )

  })

  output$tab <- DT::renderDataTable({

    req(r$results)

    val <- r$results %>%
      tidyr::pivot_longer(cols = -ID) %>%
      dplyr::pull(value)
    brks <- quantile(val, probs = seq(.05, .95, length.out = 50), na.rm = TRUE)

    r$results %>%
      DT::datatable(
        class = 'cell-border stripe',
        rownames = FALSE,
        # colnames = c("Sample ID", "MBT'5Me", "IR6Me", "CI"),
        selection = "none",
        options = list(
          pageLength = 50,
          searching = FALSE,
          lengthChange = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(r$results)-1)))
        )
      ) %>%
      DT::formatRound(
        setdiff(names(r$results), "ID"),
        digits = 1
      ) %>%
      DT::formatStyle(
        setdiff(names(r$results), "ID"),
        backgroundColor = DT::styleInterval(brks, colorRampPalette(c("#3d84a8", "white", "#f67280"))(length(brks)+1))
      )

  })


}

## To be copied in the UI
# mod_maat_prediction_ui("maat_prediction_ui_1")

## To be copied in the server
# callModule(mod_maat_prediction_server, "maat_prediction_ui_1", r)

