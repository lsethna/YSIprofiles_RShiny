## -- R Shiny App to clip and plot YSI data -- ##
# Author: Lienne Sethna

#install/load packages
install.packages("librarian")
librarian::shelf(tidyverse,shiny)

# Define UI - this controls the appearance of the app
ui <- fluidPage(
  titlePanel("YSI profile data: clip and plot"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload .csv file"),
      actionButton("loadData", "Load data"),
      hr(),
      h4("Filter data"),
      sliderInput("filter", "Clip data:", min = 0, max = 100, value = c(0, 100)),
      hr(),
      checkboxGroupInput("cols", "Select columns to display:",
                              choices = NULL),
      hr(),
      h4("Plot Options"),
      selectInput("x", "X-axis Variable", choices = NULL),
      selectInput("y", "Y-axis Variable", choices = NULL),
      actionButton("plot", "Plot")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$loadData, {
    inFile <- input$file
    if (is.null(inFile))
      return()
    
    data(read.csv(inFile$datapath))
    updateCheckboxGroupInput(session, "cols", "Select columns to display:", choices = names(data()))
    updateSelectInput(session, "x", "X-axis Variable", choices = names(data()))
    updateSelectInput(session, "y", "Y-axis Variable", choices = names(data()))
  })
  
  observeEvent(input$plot, {
    x <- input$x
    y <- input$y
    filtered_data <- subset(data(), data()[[x]] >= input$filter[1] & data()[[x]] <= input$filter[2])
    output$plot <- renderPlot({
      ggplot(filtered_data, aes_string(x = x, y = y)) +
        geom_point() +
        labs(x = x, y = y) +
        ggtitle("Scatter Plot")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)