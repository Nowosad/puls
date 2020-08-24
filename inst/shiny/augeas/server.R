# Define server logic required to draw a histogram
server = function(input, output, session) {
  outData = reactive({
    cleaner(input$file)
  })
  outVar = reactive({
    c("Blank", unique(outData()$name))
  })
  outDates = reactive({
    c(start = min(as.Date(outData()$TIMESTAMP)),
      end =   max(as.Date(outData()$TIMESTAMP)))
  })
  observe({
    updateSelectInput(session, "var1", choices = outVar()
    )})
  observe({
    updateSelectInput(session, "var2", choices = outVar()
    )})
  observe({
    updateDateRangeInput(session, "plotdaterange",
                         start = outDates()[[1]],
                         end = outDates()[[2]],

               shinyjs::delay(0,  # delay 250ms
                              updateDateRangeInput(session, "plotdaterange",
                                                   min = outDates()[[1]],
                                                   max = outDates()[[2]])
                                    ))
               })
  error_brody_sel = reactive({
    brody_error %>%
      filter(dplyr::between(as.Date(TIMESTAMP), input$errorsdaterange[1], input$errorsdaterange[2]))
  })
  output$error_brodyTable = DT::renderDataTable({
    dat_brody = DT::datatable(error_brody_sel(),
                               filter = list(position = "top", clear = FALSE),
                               options = list(search = list(regex = TRUE, caseInsensitive = FALSE))) %>%
      DT::formatStyle("problem", target = "row",
                      backgroundColor = DT::styleEqual("NaN values", "#FF4136"))
    return(dat_brody)
  })
  error_kusowo_sel = reactive({
    kusowo_error %>%
      filter(dplyr::between(as.Date(TIMESTAMP), input$errorsdaterange[1], input$errorsdaterange[2]))
  })
  output$error_kusowoTable = DT::renderDataTable({
    dat_kusowo = DT::datatable(error_kusowo_sel(),
        filter = list(position = "top", clear = FALSE),
        options = list(search = list(regex = TRUE, caseInsensitive = FALSE))) %>%
      DT::formatStyle("problem", target = "row",
                      backgroundColor = DT::styleEqual("NaN values", "#FF4136"))
    return(dat_kusowo)
  })
  error_rzecin_sel = reactive({
    rzecin_error %>%
      filter(dplyr::between(as.Date(TIMESTAMP), input$errorsdaterange[1], input$errorsdaterange[2]))
  })
  output$error_rzecinTable = DT::renderDataTable({
    dat_rzecin = DT::datatable(error_rzecin_sel(),
                               filter = list(position = "top", clear = FALSE),
                               options = list(search = list(regex = TRUE, caseInsensitive = FALSE))) %>%
      DT::formatStyle("problem", target = "row",
                      backgroundColor = DT::styleEqual("NaN values", "#FF4136"))
    return(dat_rzecin)
  })
  my_plot = reactive({
    plot_data = outData() %>%
      subset(name %in% c(input$var1, input$var2))  %>%
      filter(dplyr::between(as.Date(TIMESTAMP),
                            input$plotdaterange[1], input$plotdaterange[2]))

    time_diff = input$plotdaterange[2] - input$plotdaterange[1]
    time_diff = as.numeric(time_diff, "days")
    if (time_diff > 30){
      date_style = "%Y-%m-%d"
    } else {
      date_style = "%Y-%m-%d %H:%M:%S"
    }

    if (nrow(plot_data) > 0){
      gg = ggplot(plot_data) +
        geom_line(aes(TIMESTAMP, value, color = name)) +
        labs(x = input$xlab,
             y = input$ylab,
             color = input$legend) +
        scale_color_manual(values = c(input$col1, input$col2)) +
        scale_x_datetime(date_labels = date_style)
      return(gg)
    }
  })
  output$dataPlot = renderPlot({
    my_plot()
  })
  output$png = downloadHandler(
    filename = function() { paste("plot1", ".png", sep = "") },
    content = function(file) {
      ggsave(file, plot = my_plot(), device = "png")
    }
  )
  output$eps = downloadHandler(
    filename = function() { paste("plot1", ".eps", sep = "") },
    content = function(file) {
      ggsave(file, plot = my_plot(), device = "eps")
    }
  )
}

# Run the application
# shinyApp(ui = ui, server = server)
