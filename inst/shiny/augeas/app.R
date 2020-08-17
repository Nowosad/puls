library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(purrr)
library(puls)
library(tidyr)
library(stringr)

start_date = as.Date("2020-06-01")
end_date = as.Date("2020-06-01")

brody_files = dir("/home/jn/Documents/up-data-validation/data/Brody", pattern = ".dat", full.names = TRUE)
kusowo_files = dir("/home/jn/Documents/up-data-validation/data/Kusowo", pattern = ".dat", full.names = TRUE)
rzecin_files = dir("/home/jn/Documents/up-data-validation/data/Rzecin", pattern = ".dat", full.names = TRUE)

stacje = list(Brody = brody_files,
              Kusowo = kusowo_files,
              Rzecin = rzecin_files)

# brody_error = readRDS("/home/jn/Documents/puls/brody_error.rds")
# kusowo_error = readRDS("/home/jn/Documents/puls/kusowo_error.rds")
# rzecin_error = readRDS("/home/jn/Documents/puls/rzecin_error.rds")
brody_error = fst::read_fst("/home/jn/Documents/puls/brody_error.fst")
kusowo_error = fst::read_fst("/home/jn/Documents/puls/kusowo_error.fst")
rzecin_error = fst::read_fst("/home/jn/Documents/puls/rzecin_error.fst")

# Define UI for application that draws a histogram
ui = bootstrapPage(
    navbarPage(theme = shinytheme("simplex"),
               collapsible = TRUE,
               "Station Watch Dog",
               id = "nav",

               tabPanel("Ostrzeżenia",
                        sidebarLayout(
                            sidebarPanel(
                                dateRangeInput("errorsdaterange",
                                       "Wybrany zakres czasowy",
                                       separator = " do ",
                                       start = start_date,
                                       end = end_date,
                                       weekstart = 1,
                                       language = "pl")
                            ),
                            mainPanel(
                                h2("Brody"),
                                DT::dataTableOutput("error_brodyTable"),
                                h2("Kusowo"),
                                DT::dataTableOutput("error_kusowoTable"),
                                h2("Rzecin"),
                                DT::dataTableOutput("error_rzecinTable")
                            )
                        )
               ),

               tabPanel("Wykresy",
                   sidebarLayout(
                       sidebarPanel(
                           # selectInput("site",
                           #             "Stacja:",
                           #             choices = stacje),
                           selectInput("file",
                                       "File:",
                                       choices = stacje),
                           selectInput("var1",
                                       "Name (yellow):",
                                       choices = ""),
                           selectInput("var2",
                                       "Name (blue):",
                                       choices = ""),
                           dateRangeInput("plotdaterange",
                                          "Wybrany zakres czasowy",
                                          separator = " do ",
                                          # start = as.Date(-Inf, origin = "1970-01-01"),
                                          # end = as.Date(Inf, origin = "1970-01-01"),
                                          start = as.Date("2015-01-01"),
                                          end = Sys.Date(),
                                          weekstart = 1,
                                          language = "pl")
                       ),
                       mainPanel(
                           plotOutput("dataPlot")
                       )
                   )
               )
    )
)

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
shinyApp(ui = ui, server = server)
