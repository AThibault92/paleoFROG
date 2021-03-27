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
    fluidRow(
      h3("GDGTs indices")
    ),
    tabsetPanel(
      tabPanel("MBT5'me", echarts4r::echarts4rOutput(outputId = ns("plot_mbt"))),
      tabPanel("IR6me", echarts4r::echarts4rOutput(outputId = ns("plot_ir6"))),
      tabPanel("CI", echarts4r::echarts4rOutput(outputId = ns("plot_ci"))),
      tabPanel("table", DT::dataTableOutput(outputId = ns("tab")))
    )
  )
}

#' indices Server Function
#'
#' @noRd
mod_indices_server <- function(input, output, session, r){
  ns <- session$ns

  observeEvent(r$data,{
    if (!is.na(r$data)){
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
    }
  })

output$plot_mbt <- echarts4r::renderEcharts4r({

  if (!is.na(r$indices)){

    r$indices %>%
      echarts4r::e_chart(ID) %>%
      echarts4r::e_bar(mbt5me) %>%
      echarts4r::e_flip_coords() %>%
      echarts4r::e_legend(show = FALSE) %>%
      echarts4r::e_theme("macarons") %>%
      echarts4r::e_x_axis(name = "MBT5'me", nameLocation = "center", nameGap = 30) %>%
      echarts4r::e_toolbox() %>%
      echarts4r::e_toolbox_feature(feature = c("saveAsImage"), title = "Download") %>%
      echarts4r::e_tooltip(
        trigger = "item"
      )
  }

})

output$plot_ir6 <- echarts4r::renderEcharts4r({

  if (!is.na(r$indices)){

    r$indices %>%
      echarts4r::e_chart(ID) %>%
      echarts4r::e_bar(ir6me) %>%
      echarts4r::e_flip_coords() %>%
      echarts4r::e_legend(show = FALSE) %>%
      echarts4r::e_theme("macarons") %>%
      echarts4r::e_x_axis(name = "IR6me", nameLocation = "center", nameGap = 30) %>%
      echarts4r::e_toolbox() %>%
      echarts4r::e_toolbox_feature(feature = c("saveAsImage"), title = "Download") %>%
      echarts4r::e_tooltip(
        trigger = "item"
      )
  }

})

output$plot_ci <- echarts4r::renderEcharts4r({

  if (!is.na(r$indices)){

    r$indices %>%
      echarts4r::e_chart(ID) %>%
      echarts4r::e_bar(ci) %>%
      echarts4r::e_flip_coords() %>%
      echarts4r::e_legend(show = FALSE) %>%
      echarts4r::e_theme("macarons") %>%
      echarts4r::e_x_axis(name = "CI", nameLocation = "center", nameGap = 30) %>%
      echarts4r::e_toolbox() %>%
      echarts4r::e_toolbox_feature(feature = c("saveAsImage"), title = "Download") %>%
      echarts4r::e_tooltip(
        trigger = "item"
      )
  }

})

output$tab <- DT::renderDataTable({

  r$indices

})

}

## To be copied in the UI
# mod_indices_ui("indices_ui_1")

## To be copied in the server
# callModule(mod_indices_server, "indices_ui_1")

