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
