# Load packages.
library(shiny)
library(bslib)
library(XML)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "fstrava",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Slider for the number of bins ----
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 5,
      max = 50,
      value = 30
    )
  ),
  # Output: Histogram ----
  plotOutput(outputId = "distPlot")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    # Load in example gpx file.
    gpx_df <- htmlTreeParse(file = "data/4779803360.gpx", useInternalNodes = TRUE)
    
    # Extract elevation.
    elevation <- xpathSApply(doc = gpx_df, path = "//trkpt/ele", fun = xmlValue)
    
    x    <- as.numeric(elevation)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#fc4c02", border = "white",
         xlab = "Elevation",
         main = "Histogram of elevation during a single run.")
    
    
    
  })
  
}

# Call the app.
shinyApp(ui = ui, server = server)