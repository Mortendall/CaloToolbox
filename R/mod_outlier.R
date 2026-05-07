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
      col_widths = c(4, 8),
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
          title = "Model Evaluation Plots",
          full_screen = T,
          bslib::nav_panel(
            "QC plot",
            bslib::card_body(
              shiny::uiOutput(
                outputId = ns("xy_ui")
              )
            )
          ),
          bslib::nav_panel(
            "Box plot",
            bslib::card_body(
              shiny::uiOutput(
                outputId = ns("boxplot")
              )
            )
          ),
          bslib::nav_panel(
            "Mass change",
            bslib::card_body(
              shiny::uiOutput(
                outputId = ns("mass_change")
              )
            )
          ),
          bslib::nav_panel(
            "Outlier test",
            bslib::card_body(
              shiny::uiOutput(
                outputId = ns("outlier_test")
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
mod_outlier_server <- function(id, parentsession, dataobject) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    ##### process calR file####

    # show button for processing once files are uploaded
    output$process <- shiny::renderUI({
      req(dataobject$calr)
      req(dataobject$session)
      shiny::tagList(
        shinyWidgets::actionBttn(
          inputId = ns("process_start"),
          label = "make QC plots",
          style = "jelly"
        ),
        shiny::selectizeInput(
          inputId = ns("select_parameter"),
          label = "Select parameter for plots",
          choices = c(
            "energy expenditure",
            "energy balance",
            "food intake",
            "vo2"
          ),
          selected = "energy expenditure",
          options = list(dropdownParent = 'body')
        ),
        shiny::selectizeInput(
          inputId = ns("select_individual"),
          label = "select individual to exclude",
          choices = dataobject$summary$subject.id,
          options = list(dropdownParent = 'body')
        ),
        shinyWidgets::actionBttn(
          inputId = ns("exclude_individual"),
          style = "jelly",
          label = "exclude"
        )
      )
    })

    # summarize data
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

    ##### visualize XY plot####
    output$xy_ui <- shiny::renderUI({
      req(dataobject$summary)


      # select plot to display

      shiny::tagList(
        # display plot
        plotly::plotlyOutput(
          outputId = ns("xy_plot")
        )
      )
    })

    output$xy_plot <- plotly::renderPlotly({
      req(dataobject$summary)

      # check what parameter is selected
      if (input$select_parameter == "energy expenditure") {
        xy_plot <- xy_plotter(
          summary_calr = dataobject$summary,
          color_key = dataobject$color_key,
          y_text = "Energy Expenditure",
          plot_parameter = "ee_mean"
        )
      } else if (input$select_parameter == "energy balance") {
        xy_plot <- xy_plotter(
          summary_calr = dataobject$summary,
          color_key = dataobject$color_key,
          y_text = "Energy Balance",
          plot_parameter = "eb_mean"
        )
      } else if (input$select_parameter == "vo2") {
        xy_plot <- xy_plotter(
          summary_calr = dataobject$summary,
          color_key = dataobject$color_key,
          y_text = "VO2",
          plot_parameter = "vo2_mean"
        )
      } else {
        xy_plot <- xy_plotter(
          summary_calr = dataobject$summary,
          color_key = dataobject$color_key,
          y_text = "Food Intake",
          plot_parameter = "feed_mean"
        )
      }
    })

    #Exclude subject when button is pressed

    shiny::observeEvent(
      input$exclude_individual,{
        dataobject$summary <- dataobject$summary |>
          dplyr::filter(!subject.id == input$select_individual)
      }
    )


    #####Boxplot UI####
    output$boxplot <- shiny::renderUI({
      req(dataobject$summary)
      plotly::plotlyOutput(
        outputId = ns("boxplot_graph")
      )
    })

    output$boxplot_graph<- plotly::renderPlotly({
      req(dataobject$summary)

      if (input$select_parameter == "energy expenditure") {
        box_plot <- boxplot_generator(
          calr_summary = dataobject$summary,
          calr_circadian = dataobject$circadian,
          parameter = "ee",
          axis_text = "Energy Expenditure",
          color_list = dataobject$color_key
        )
      } else if (input$select_parameter == "energy balance") {
        box_plot <- boxplot_generator(
          calr_summary = dataobject$summary,
          calr_circadian = dataobject$circadian,
          parameter = "eb",
          axis_text = "Energy Balance",
          color_list = dataobject$color_key
        )
      } else if (input$select_parameter == "vo2") {
        box_plot <- boxplot_generator(
          calr_summary = dataobject$summary,
          calr_circadian = dataobject$circadian,
          parameter = "vo2",
          axis_text = "VO2",
          color_list = dataobject$color_key
        )
      } else {
        box_plot <- boxplot_generator(
          calr_summary = dataobject$summary,
          calr_circadian = dataobject$circadian,
          parameter = "feed",
          axis_text = "Food Intake",
          color_list = dataobject$color_key
        )
      }
    })

    output$mass_change <- shiny::renderUI({
      req(dataobject$summary)
      plotly::plotlyOutput(
        outputId = ns("mass_change_plot")
      )
    })
    #####Mass change plot####
    output$mass_change_plot <- plotly::renderPlotly({
      mass_change_graph <- ggplot2::ggplot(
        dataobject$summary,
        ggplot2::aes(
          x = mass.change,
          y = eb_mean,
          color = group
        )
      )+
        ggplot2::geom_point(
          size = 6,
          ggplot2::aes(text = subject.id)
        )+
        ggplot2::theme_bw()+
        ggplot2::xlab("Mass Change")+
        ggplot2::ylab("Energy Balance")+
        ggplot2::scale_color_manual(values = dataobject$color_key$colors)

    })

    #####Outlier test####
    output$outlier_test <- shiny::renderUI({
      req(dataobject$summary)
      shiny::tagList(
        #make button to run outlier analysis.
        shinyWidgets::actionBttn(
          inputId = ns("outlier_run"),
          label = "Run outlier test for select variable",
          style = "jelly"
        ),
        #show result of outlier test
        shiny::uiOutput(
          outputId = ns("outliertest")
        )
      )
    })

    shiny::observeEvent(
      input$outlier_run,{
        model_string <- generate_formula(input$select_parameter)
        dataobject$model <- lm(data = dataobject$summary,
                               formula = model_string)
        names(dataobject$model$residuals)<- dataobject$summary$subject.id
      }
    )

    #render UI of model data

    output$outliertest <- shiny::renderUI({
      req(dataobject$model)
      shiny::tagList(
          shiny::uiOutput(
            outputId = ns("residual_test")
          ),
          shiny::plotOutput(
            outputId = ns("model_plots")
          )
        )
    })

    #calculate residuals and render problematic ones
    output$residual_test <- shiny::renderUI({
      dataobject$residuals <- stats::rstandard(dataobject$model)
      if(length(which(abs(dataobject$residuals) > 3))==0){
        shiny::textOutput(
          outputId = ns("no_residuals"))
      }
      else{
        shiny::tagList(
          shiny::textOutput(
          outputId = ns("residual_text")
        ),
        shiny::tableOutput(
          outputId = ns("residual_table")
        ))
      }
    })

    output$residual_text <- shiny::renderText({
      "The following individuals have absolute
      standardized residuals greater than 3:"
    })

    output$no_residuals <- shiny::renderText({
      "No individual has absolute standardized residuals greater than 3"
    })

    output$residual_table <- shiny::renderTable({
      residual_table <- dataobject$summary |>
        dplyr::mutate(
          standardized_residuals = dataobject$residuals
        ) |>
        dplyr::filter(abs(standardized_residuals)>3) |>
        dplyr::select(subject.id,
                      standardized_residuals)
      shiny::tagList(
        residual_table)
    })

    #make plots of model

    output$model_plots <- shiny::renderPlot({
      req(dataobject$model)
      par(mfrow = c(2,2))
      plot(dataobject$model)

    })
  })
}
