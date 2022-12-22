library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("LoESs coLorimetry sIgnal Enhancement"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            tabPanel("Input",
                fileInput("input_file", "Choose CSV file", accept = ".csv"),
                checkboxInput("input_header", "CSV file has header?", TRUE),
                HTML("<hr>"),
                fluidRow(
                  column(12, align = 'center',
                    actionButton("example_data", "Load example data")
                   )
                 ),
                ),
            tabPanel("Plot",
                sliderInput("cycles", "Number of enhancement cycles:",
                                 min = 0, max = 10, value = 3),
                radioButtons("orientation", "Plot orientation:",
                              c("Portrait" = "portrait",
                                "Landscape" = "landscape"))),
          ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("leslie_plot")
        )
    ),
    HTML("<hr>
           <div align = 'center'><small>This software comes WITHOUT ANY WARRANTY.</small>
          </div>")
)
