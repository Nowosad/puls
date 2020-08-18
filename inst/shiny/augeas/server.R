# Define server logic required to draw a histogram
server = function(input, output, session) {
  outData = reactive({
    cleaner(input$file)
  })
  outVar = reactive({
    c("Blank", unique(outData()$name))
  })
  observe({
    updateSelectInput(session, "var1", choices = outVar()
    )})
  observe({
    updateSelectInput(session, "var2", choices = outVar()
    )})
  error_brody_sel = reactive({
    brody_error %>%
      filter(dplyr::between(as.Date(TIMESTAMP), input$errorsdaterange[1], input$errorsdaterange[2]))
  })
  output$error_brodyTable = DT::renderDataTable(
    error_brody_sel(),
    filter = list(position = "top", clear = FALSE),
    options = list(search = list(regex = TRUE, caseInsensitive = FALSE))
  )
  error_kusowo_sel = reactive({
    kusowo_error %>%
      filter(dplyr::between(as.Date(TIMESTAMP), input$errorsdaterange[1], input$errorsdaterange[2]))
  })
  output$error_kusowoTable = DT::renderDataTable(
    error_kusowo_sel(),
    filter = list(position = "top", clear = FALSE),
    options = list(search = list(regex = TRUE, caseInsensitive = FALSE))
  )
  error_rzecin_sel = reactive({
    rzecin_error %>%
      filter(dplyr::between(as.Date(TIMESTAMP), input$errorsdaterange[1], input$errorsdaterange[2]))
  })
  output$error_rzecinTable = DT::renderDataTable(
    error_rzecin_sel(),
    filter = list(position = "top", clear = FALSE),
    options = list(search = list(regex = TRUE, caseInsensitive = FALSE))
  )
  output$dataPlot = renderPlot({
    plot_data1 = outData() %>%
      subset(name == input$var1)  %>%
      filter(dplyr::between(as.Date(TIMESTAMP), input$plotdaterange[1], input$plotdaterange[2]))
    plot_data2 = outData() %>%
      subset(name == input$var2)  %>%
      filter(dplyr::between(as.Date(TIMESTAMP), input$plotdaterange[1], input$plotdaterange[2]))
    ggplot() +
      geom_line(data = plot_data1, aes(TIMESTAMP, value), color = "#FFD662FF") +
      geom_line(data = plot_data2, aes(TIMESTAMP, value), color = "#00539CFF")
  })
}

# Run the application
# shinyApp(ui = ui, server = server)
