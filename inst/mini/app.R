library(shiny)
library(dygraphs)
library(webshot)
library(reprexsnapshot)

ui <- fluidPage(
  titlePanel("dygraph to png"),
  modUI("plop")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   callModule(mod,"plop")
}

# Run the application
shinyApp(ui = ui, server = server)

