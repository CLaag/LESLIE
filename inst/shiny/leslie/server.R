#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  observe({
   if (is.null(input$input_file)) {
     df <- NULL
     if(input$example_data) {
       data(LESLIE_profile, envir = environment())
       df <- LESLIE_profile
     }

   } else {
     df <- read.csv(input$input_file$datapath, header = input$input_header)

   }

   ## plot window
   if (!is.null(df)) {
      output$leslie_plot <- renderPlot({
        plot_colProfile(
          data = df,
          depth_top = NULL,
          depth_bottom = NULL,
          cycles = 1:input$cycles,
          orientation = input$orientation,
          plot = TRUE,
          main = if (input$example_data) "Example data" else ""
        )
    })
    }
  })
}
