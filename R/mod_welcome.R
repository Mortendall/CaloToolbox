#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = 4,
        shiny::column(12),
        shiny::column(12,
                      bslib::card(
                        bslib::card_header("Welcome to the CaloToolbox"),
                        bslib::card_body(
                          bslib::card_body(
                            htmltools::span(
                              "A collection of tools for further processing indirect calorimetry data processed with ",
                              shiny::a("CalR", href = "https://calrapp.org/")
                            )
                          )

                        )
                      ))),
    bslib::layout_columns(
      col_widths = 4,
      shiny::column(12,
                    bslib::card(
                      bslib::card_header("The Outlier Tool"),
                      bslib::card_body("Detect outliers and inspect data",
                                       shinyWidgets::actionBttn(
                                         inputId = ns("outlier"),
                                         style = "jelly",
                                         label = "launch outlier tool",size = "s",
                                         block = F
                                       ))

                    ),
                    bslib::card(
                      bslib::card_header("Manual Food Joiner"),
                      bslib::card_body("Add manually weighed daily food intake to your dataset")
                    )
                    ),
      shiny::column(12,
                    bslib::card(
                      bslib::card_header("Trim drifting sensor"),
                      bslib::card_body("")
                    )
                    #,
                    #not really necessary as this can be done with the macro
                    # bslib::card(
                    #   bslib::card_header("CalR paster"),
                    #   bslib::card_body("Paste together CalR runs that were started
                    #                    and stopped, and zero cumulative values")
                    # )
                    ),
      shiny::column(12,
                    bslib::card(
                      bslib::card_header("EMM Multiple Comparison Tester"),
                      bslib::card_body("Perform a multiple comparison test of
                                       Estimated Marginal Means")
                    ))
    ))

}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id, parentsession){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    shiny::observeEvent(
      input$outlier,{
        shiny::updateTabsetPanel(
          session = parentsession,
          inputId = "inTabset",
          selected = "outlier"

        )
      }
    )
  })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
