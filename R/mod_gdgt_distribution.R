#' gdgt_distribution UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gdgt_distribution_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      h3("Visualisation of the distribution of GDGTs")
    ),
    sidebarLayout(
      sidebarPanel(
        uiOutput(outputId = ns("uiout_select_sample"))
      ),
      mainPanel(
        echarts4r::echarts4rOutput(outputId = ns("plot"))
      )
    )
  )
}

#' gdgt_distribution Server Function
#'
#' @noRd
mod_gdgt_distribution_server <- function(input, output, session, r){
  ns <- session$ns

  output$uiout_select_sample <- renderUI({

    if (!is.na(r$data)){
      selectInput(inputId = ns("sample"), label = "Choose your sample(s)", choices = r$data$ID, multiple = TRUE)
    }

  })

  output$plot <- echarts4r::renderEcharts4r({

    if (input$sample != ""){

      if (length(input$sample) == 1) {
        r$data %>%
          dplyr::filter(ID == input$sample) %>%
          tidyr::pivot_longer(cols = -ID) %>%
          dplyr::mutate(name = dplyr::case_when(
            name == "i_a" ~ "Ia",
            name == "i_b" ~ "Ib",
            name == "i_c" ~ "Ic",
            name == "ii_a" ~ "IIa",
            name == "ii_a_p" ~ "IIa'",
            name == "ii_b" ~ "IIb",
            name == "ii_b_p" ~ "IIb'",
            name == "ii_c" ~ "IIc",
            name == "ii_c_p" ~ "IIc'",
            name == "iii_a" ~ "IIIa",
            name == "iii_a_p" ~ "IIIa'",
            name == "iii_b" ~ "IIIb",
            name == "iii_b_p" ~ "IIIb'"
          ), value = value * 100) %>%
          echarts4r::e_charts(name) %>%
          echarts4r::e_bar(value) %>%
          echarts4r::e_title("Relatives abundances of each GDGT", input$select_sample) %>%
          echarts4r::e_legend(show = FALSE) %>%
          echarts4r::e_y_axis(name = "Relative abundance (%)", nameLocation = "center", nameGap = 30) %>%
          echarts4r::e_toolbox() %>%
          echarts4r::e_toolbox_feature(feature = c("saveAsImage"), title = "Download") %>%
          echarts4r::e_tooltip(
            trigger = "item",
            formatter = htmlwidgets::JS("
    function(params){
      return('3-OH FA: ' + params.value[0] + '<br />Relative abundance : ' + Math.round(params.value[1]*10)/10)
    }
    ")
          ) %>%
          echarts4r::e_theme("macarons")
      } else {

        r$data %>%
          dplyr::filter(ID %in% input$sample) %>%
          tidyr::pivot_longer(cols = -ID) %>%
          dplyr::mutate(name = dplyr::case_when(
            name == "i_a" ~ "Ia",
            name == "i_b" ~ "Ib",
            name == "i_c" ~ "Ic",
            name == "ii_a" ~ "IIa",
            name == "ii_a_p" ~ "IIa'",
            name == "ii_b" ~ "IIb",
            name == "ii_b_p" ~ "IIb'",
            name == "ii_c" ~ "IIc",
            name == "ii_c_p" ~ "IIc'",
            name == "iii_a" ~ "IIIa",
            name == "iii_a_p" ~ "IIIa'",
            name == "iii_b" ~ "IIIb",
            name == "iii_b_p" ~ "IIIb'"
          ), value = value * 100) %>%
          dplyr::group_by(ID) %>%
          echarts4r::e_charts(name) %>%
          echarts4r::e_bar(value) %>%
          echarts4r::e_title("Relatives abundances of each GDGT", input$select_sample) %>%
          echarts4r::e_legend(show = TRUE) %>%
          echarts4r::e_y_axis(name = "Relative abundance (%)", nameLocation = "center", nameGap = 30) %>%
          echarts4r::e_toolbox() %>%
          echarts4r::e_toolbox_feature(feature = c("saveAsImage"), title = "Download") %>%
          echarts4r::e_tooltip(
            trigger = "item",
            formatter = htmlwidgets::JS("
    function(params){
      return('3-OH FA: ' + params.value[0] + '<br />Relative abundance : ' + Math.round(params.value[1]*10)/10)
    }
    ")
          ) %>%
          echarts4r::e_theme("macarons")



      }



    }

  })

}

## To be copied in the UI
# mod_gdgt_distribution_ui("gdgt_distribution_ui_1")

## To be copied in the server
# callModule(mod_gdgt_distribution_server, "gdgt_distribution_ui_1", r)

