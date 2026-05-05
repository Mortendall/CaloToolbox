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
            "Upload CalR- and session file"
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
            ),
            shiny::uiOutput(
              outputId = ns("process")
            )
          )
        )
      ),
      shiny::column(
        12,
        bslib::navset_card_tab(
              title = "",
              full_screen = T,
              bslib::nav_panel(
                "Model evaluation plots",
                bslib::card_title("QC plot"),
                bslib::card_body(
                  shiny::uiOutput(
                    outputId = ns("xy_ui")
                  )
                )
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

    #####CalR upload####
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

          #determine resolution
          dataobject$res <-as.numeric(processed_data$Date.Time[2]-processed_data$Date.Time[1])
          shiny::showNotification(
            "Data succesfully uploaded and converted to CalR format")
        }

        else if("Date.Time"%in% colnames(raw_data)){
          dataobject$calr <- raw_data
          #determine resolution
          dataobject$res <-as.numeric(raw_data$Date.Time[2]-raw_data$Date.Time[1])

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

    #####Session file upload####
    #recognize upload of session file
    shiny::observeEvent(
      input$session_upload,{
        ext <- tools::file_ext(input$session_upload)
        ext <- tolower(ext)
        req(file)
        validate(need(ext == "csv",
                      "Please upload an csv file"))


        session_file<- vroom::vroom(input$session_upload$datapath,
                                 show_col_types = F)

        #check csv is right format
        if(all(c("group_names", "diet_names","dietCal") %in% colnames(session_file))){

          #save session file
          dataobject$session <- session_file

          #add groups
          dataobject$group_info <- group_assigner(session_file)
          shiny::showNotification("Session file succesfully uploaded")
        }
        else{
          shinyWidgets::sendSweetAlert(
            title = "Error in upload",
            text = "Uploaded file has wrong headers - are you sure you
                    uploaded the right file?",
            type = "error"
          )
        }

      })

    #####process calR file####

    #show button for processing once files are uploaded
    output$process <- shiny::renderUI({
      req(dataobject$calr)
      req(dataobject$session)
      shinyWidgets::actionBttn(
        inputId = ns("process_start"),
        label = "make QC plot",
        style = "jelly"
      )
    })

    #summarize data
    shiny::observeEvent(
      input$process_start,{
        res <- calculate_res(dataobject$calr)
        dataobject$summary <- generate_summary_data(dataobject$calr,
                                                    dataobject$session,
                                                    dataobject$group_info,
                                                    res)
      }
    )

    #####visualize XY plot####
    output$xy_ui <- shiny::renderUI({
      req(dataobject$summary)
      shiny::tagList(
        shiny::selectInput(
          inputId = ns("select_parameter"),
          label = "Select parameter for plots",
          choices = c("energy expenditure")),
        plotly::plotlyOutput(
          outputId = ns("xy_plot")
        )
      )

    })

    # output$xy_plot <- plotly::renderPlotly({
    #   plotly::plot_ly(
    #     data = dataobject$summary,
    #     x = ~Total.Mass,
    #     y = ~
    #   )
    # })


  })
}
