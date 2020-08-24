start_date = as.Date("2020-06-01")
end_date = as.Date("2020-06-01")

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
                                         # start = as.Date("2015-01-01"),
                                         # end = Sys.Date(),
                                         weekstart = 1,
                                         language = "pl"),
                          textInput("xlab",
                                    "X axis label",
                                    value = "Termin"
                                    ),
                          textInput("ylab",
                                    "Y axis label",
                                    value = "Wartość"
                          ),
                          colourInput("col1",
                                    "Color 1",
                                    value = "#FFD662FF"
                          ),
                          colourInput("col2",
                                    "Color 2",
                                    value = "#00539CFF"
                          ),
                          textInput("legend",
                                    "Legend title",
                                    value = "Legenda"
                          ),
                          downloadButton("png", "png"),
                          downloadButton("eps", "eps"),
                        ),
                        mainPanel(
                          plotOutput("dataPlot")
                        )
                      )
             )
  )
)
