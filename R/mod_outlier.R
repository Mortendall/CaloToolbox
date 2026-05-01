#' outlier UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_outlier_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(4,8),
      shiny::column(
        12,
        bslib::card(
          bslib::card_header(
            ""
          ),
          bslib::card_body(
            shiny::fileInput(
              inputId = ns("calr_file"),
              label = "Upload a CalR file.",
              multiple = F,
              accept = ".csv"
            ),
            shiny::uiOutput(
              outputId = ns("session")
            )
          )
        )
      ),
      shiny::column(
        12,
        bslib::navset_card_tab(
              title = "QC plots",
              full_screen = T,
              bslib::nav_panel(
                "Model evaluation plots",
                bslib::card_title("QC plot"),
              )

          )
        )
      )
    )

}

#' outlier Server Functions
#'
#' @noRd
mod_outlier_server <- function(id, parentsession, dataobject){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #register a calR file is uploaded
    shiny::observeEvent(
      input$calr_file,{
        ext <- tools::file_ext(input$calr_file)
        ext <- tolower(ext)
        req(file)
        validate(need(ext == "csv",
                      "Please upload an csv file"))
        raw_data <- vroom::vroom(input$calr_file$datapath,
                                 show_col_types = F)

        #check if this is a processed or macro-exported calR format

        if ("DurationMin_1" %in% colnames(raw_data)){

          processed_data <- convert_to_calr(calr_file = raw_data,
                                            calr_headers = dataobject$calr_headers)
          dataobject$calr <- processed_data
          shiny::showNotification(
            "Data succesfully uploaded and converted to CalR format")
        }

        else if("Date.Time"%in% colnames(raw_data)){
          dataobject$calr <- raw_data
          shiny::showNotification(
            "Data succesfully uploaded. Data recognized as CalR format")
        }

        else{
          shinyWidgets::sendSweetAlert(
            title = "Error in upload",
            text = "Uploaded file has wrong headers - are you sure you
                    uploaded the right file?",
            type = "error"
          )
        }
      }
    )

    #display session upload once calR file is succesfully uploaded
    output$session <- shiny::renderUI({
      req(dataobject$calr)
      shiny::fileInput(
        inputId = ns("session_upload"),
        label = "upload a calR session file",
        multiple = F,
        accept = ".csv"
      )
    })
  })
}

## To be copied in the UI
# mod_outlier_ui("outlier_1")

## To be copied in the server
# mod_outlier_server("outlier_1")
