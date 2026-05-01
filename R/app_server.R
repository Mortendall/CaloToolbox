#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  parentsession <- session
  dataobject <- shiny::reactiveValues()
  dataobject$calr_headers <- readRDS(here::here("data/calr_headers.rds"))
  mod_welcome_server("welcome_1", parentsession)
  mod_outlier_server("outlier_1", parentsession, dataobject)
}
