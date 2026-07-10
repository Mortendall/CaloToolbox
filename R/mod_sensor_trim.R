#' sensor_trim UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sensor_trim_ui <- function(id) {
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
        ),
        bslib::card(
          bslib::card_header(
            "Load in example"
          ),
          bslib::card_body(
            shinyWidgets::actionBttn(
              inputId = ns("example"),
              label = "Load in example data",
              style = "jelly"
            )
          )
        )
      ),
      shiny::column(
        width = 12,
        bslib::card(
          shiny::uiOutput(
            outputId = ns("graph")
          )
        ),
        bslib::card(
          shiny::uiOutput(
            outputId = ns("controls")
          )
        )
      )
    )
  )
}

#' sensor_trim Server Functions
#'
#' @noRd
mod_sensor_trim_server <- function(id, parentsession, dataobject){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #####Load example data####
    shiny::observeEvent(
      input$example,{
        exampledata <- readRDS(here::here("data/calr_example.rds"))
        dataobject$calr <- as.data.frame(exampledata$calr)
        dataobject$session <- exampledata$session
        dataobject$group_info <- as.data.frame(exampledata$group_info)
        dataobject$res <- 3

        #process data
        #calculate resolution
        res <- calculate_res(dataobject$calr)

        #trim data, add circadian info, etc.
        dataobject$calr_trimmed <- trim_calr_data(
          dataobject$calr,
          dataobject$session,
          dataobject$group_info,
          dataobject$res
        )


        #calculate summary stats
        dataobject$summary <- generate_summary_data(
          dataobject$calr_trimmed,
          dataobject$session,
          dataobject$group_info,
          dataobject$res
        )

        #calculate circadian summary
        dataobject$circadian <- circadian_summary(
          dataobject$calr_trimmed,
          dataobject$res
        )

        #pull colors from session
        dataobject$color_key <- pull_colors(dataobject$session)

        shiny::showNotification("Succesfully loaded example data")
      }
    )

    ##### CalR upload####
    # register a calR file is uploaded
    shiny::observeEvent(
      input$calr_file,
      {
        ext <- tools::file_ext(input$calr_file)
        ext <- tolower(ext)
        req(file)
        validate(need(
          ext == "csv",
          "Please upload an csv file"
        ))
        raw_data <- vroom::vroom(input$calr_file$datapath,
                                 show_col_types = F
        )

        # check if this is a processed or macro-exported calR format

        if ("DurationMin_1" %in% colnames(raw_data)) {
          processed_data <- convert_to_calr(
            calr_file = raw_data,
            calr_headers = dataobject$calr_headers
          )

          dataobject$calr <- processed_data

          # determine resolution
          dataobject$res <- as.numeric(processed_data$Date.Time[2] - processed_data$Date.Time[1])
          shiny::showNotification(
            "Data succesfully uploaded and converted to CalR format"
          )
        } else if ("Date.Time" %in% colnames(raw_data)) {
          dataobject$calr <- raw_data
          # determine resolution
          dataobject$res <- as.numeric(raw_data$Date.Time[2] - raw_data$Date.Time[1])

          shiny::showNotification(
            "Data succesfully uploaded. Data recognized as CalR format"
          )
        } else {
          shinyWidgets::sendSweetAlert(
            title = "Error in upload",
            text = "Uploaded file has wrong headers - are you sure you
                    uploaded the right file?",
            type = "error"
          )
        }
      }
    )

    # display session upload once calR file is succesfully uploaded
    output$session <- shiny::renderUI({
      req(dataobject$calr)
      shiny::fileInput(
        inputId = ns("session_upload"),
        label = "upload a calR session file",
        multiple = F,
        accept = ".csv"
      )
    })

    ##### Session file upload####
    # recognize upload of session file
    shiny::observeEvent(
      input$session_upload,
      {
        ext <- tools::file_ext(input$session_upload)
        ext <- tolower(ext)
        req(file)
        validate(need(
          ext == "csv",
          "Please upload an csv file"
        ))


        session_file <- vroom::vroom(input$session_upload$datapath,
                                     show_col_types = F
        )

        # check csv is right format
        if (all(c("group_names", "diet_names", "dietCal") %in% colnames(session_file))) {
          # save session file
          dataobject$session <- session_file

          # add groups
          dataobject$group_info <- group_assigner(session_file)
          shiny::showNotification("Session file succesfully uploaded")
        } else {
          shinyWidgets::sendSweetAlert(
            title = "Error in upload",
            text = "Uploaded file has wrong headers - are you sure you
                    uploaded the right file?",
            type = "error"
          )
        }
      }
    )

    #####process data ui####
    output$process <- shiny::renderUI({
      req(dataobject$session)
      shinyWidgets::actionBttn(
        inputId = ns("process_start"),
        label = "process data",
        style = "jelly"
      )
    })


    ##### summarize data####
    shiny::observeEvent(
      input$process_start,
      {
        #calculate resolution
        res <- calculate_res(dataobject$calr)

        #trim data, add circadian info, etc.
        dataobject$calr_trimmed <- trim_calr_data(
          dataobject$calr,
          dataobject$session,
          dataobject$group_info,
          dataobject$res
        )


        #calculate summary stats
        dataobject$summary <- generate_summary_data(
          dataobject$calr_trimmed,
          dataobject$session,
          dataobject$group_info,
          dataobject$res
        )

        #calculate circadian summary
        dataobject$circadian <- circadian_summary(
          dataobject$calr_trimmed,
          dataobject$res
        )

        #pull colors from session
        dataobject$color_key <- pull_colors(dataobject$session)
      }
    )

    #####graph####

    output$graph <- shiny::renderUI({
      req(dataobject$calr)
      shiny::tagList(
        shiny::selectizeInput(
          inputId = ns("select_parameter"),
          label = "Select parameter for plots",
          choices = c(
            "body mass",
            "RER",
            "energy expenditure",
            "energy balance",
            "food intake",
            "vo2"

          ),
          selected = "RER",
          options = list(dropdownParent = 'body')
        ),
      plotly::plotlyOutput(
        outputId = ns("graph_server")
      ))
    })

    output$graph_server<- plotly::renderPlotly({

      req(dataobject$calr_trimmed)
      #check what parameter is selected
      # check what parameter is selected
      if (input$select_parameter == "RER") {
        trace_plotter(
          trimmed_calr = dataobject$calr_trimmed,
          y_text = "RER",
          plot_parameter = "rer"
        )
      } else if (input$select_parameter == "energy balance") {
        trace_plotter(
          trimmed_calr = dataobject$calr_trimmed,
          y_text = "Energy Balance",
          plot_parameter = "eb"
        )
      }else if (input$select_parameter == "energy expenditure") {
        trace_plotter(
          trimmed_calr = dataobject$calr_trimmed,
          y_text = "Energy Expenditure",
          plot_parameter = "ee"
        )
      } else if (input$select_parameter == "vo2") {
        trace_plotter(
          trimmed_calr = dataobject$calr_trimmed,
          y_text = "VO2",
          plot_parameter = "vo2"
        )
      } else if (input$select_parameter == "food intake") {
        trace_plotter(
          trimmed_calr = dataobject$calr_trimmed,
          y_text = "Feed intake",
          plot_parameter = "feed"
        )
      }else {
        trace_plotter(
          trimmed_calr = dataobject$calr_trimmed,
          y_text = "Body mass",
          plot_parameter = "subject.mass"
        )
      }


    })

    #####controls for plot####
    output$controls <- shiny::renderUI({
      req(dataobject$calr_trimmed)
      req(input$select_parameter)
      min_value <- extract_value(dataobject$calr_trimmed,
                                 input$select_parameter,
                                 "min")
      max_value <- extract_value(dataobject$calr_trimmed,
                                 input$select_parameter,
                                 "max")

      shiny::tagList(
        shiny::selectizeInput(
          inputId = ns("select_individual"),
          label = "select individual to exclude",
          choices = dataobject$summary$subject.id,
          options = list(dropdownParent = 'body')
        ),
        shiny::sliderInput(
          inputId = ns("select_range"),
          label = "Select range to keep",
          min = min_value,
          max = max_value,
          value = c(min_value, max_value),
          round = -2,

        ),
        shinyWidgets::actionBttn(
          inputId = ns("trim_parameter"),
          style = "jelly",
          label = "Trim value for selected cage"
        ),
        shinyWidgets::downloadBttn(
          outputId = ns("download_calR"),
          label = "download modified CalR file",
          style = "jelly"
        )
      )
    })

    #####Trim value####
    shiny::observeEvent(
      input$trim_parameter,{
        parameter <- extract_variable(input$select_parameter)

        dataobject$calr_trimmed <- dataobject$calr_trimmed |>
          dplyr::filter(
              subject.id != input$select_individual |
                (
                .data[[parameter]] >= input$select_range[1] &
                .data[[parameter]] <= input$select_range[2]
            ))

        #calculate summary stats
        dataobject$summary <- generate_summary_data(
          dataobject$calr_trimmed,
          dataobject$session,
          dataobject$group_info,
          dataobject$res
        )

        #calculate circadian summary
        dataobject$circadian <- circadian_summary(
          dataobject$calr_trimmed,
          dataobject$res
        )
      }
    )

    #####Download data####
    output$download_calR <- shiny::downloadHandler(
      filename = function(){
        paste0(Sys.Date(), "_calr.csv")
      },
      content = function(file){
        selected_columns <- colnames(dataobject$calr_trimmed) %in% colnames(dataobject$calr)

        filedata <- dataobject$calr_trimmed[selected_columns] |>
          dplyr::mutate(
            cage = subject.id,
            xyamb = NA,
            body.temp = NA,
            C13 = NA,
            hour = lubridate::floor_date(Date.Time, unit = "hour"),
            day = lubridate::floor_date(Date.Time, unit = "day")
          ) |>
          dplyr::select(colnames(dataobject$calr))
        vroom::vroom_write(x = filedata,
                           file = file,
                           delim = ",",quote = "all")
      }
    )

  })
}

## To be copied in the UI
# mod_sensor_trim_ui("sensor_trim_1")

## To be copied in the server
# mod_sensor_trim_server("sensor_trim_1")
