library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Explore song popularity"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      fluidRow(numericInput("mid", label=h3("Enter media_id: "), value=126024231)),
      fluidRow(verbatimTextOutput("info"))
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))