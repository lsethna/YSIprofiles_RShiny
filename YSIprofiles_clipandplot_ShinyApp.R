## -- R Shiny App to clip and plot YSI data -- ##
# Author: Lienne Sethna

#install/load packages
install.packages("librarian")
librarian::shelf(readxl,tidyverse,shiny)

#read in the YSI metadata to be displayed on the Shiny app instruction page
meta_dat <- read_excel("YSIdat_metadata.xlsx")

# Define UI - this controls the appearance of the app
ui <- fluidPage(
  titlePanel("DataFrame Modifier & Plotter"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File"),
      actionButton("loadData", "Load Data"),
      hr(),
      checkboxGroupInput("cols", "Select Columns to Display:",
                         choices = NULL),
      hr(),
      h4("Filter Data"),
      sliderInput("filter", "Filter Data:", min = 0, max = 100, value = c(0, 100)),
      hr(),
      h4("Plot Options"),
      selectInput("x", "X-axis Variable", choices = NULL),
      selectInput("y", "Y-axis Variable", choices = NULL),
      actionButton("plot", "Plot")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions", 
                 h2("Instructions"),
                 p("1. Ensure the column names in your YSI profile data match the metadata table below."),
                 p("2. Upload the YSI profile data using 'Upload CSV File' button. Only .csv files will work!"),
                 p("3. After uploading, select which lake to view and clip."),
                 p("4. Clip the data in the 'Clip profile' tab."),
                 p("5. Click on the 'Plot' button to generate the plot."),
                 p("6. View the plot in the 'Plot' tab."),
                 tableOutput("meta_dat")
        ),
        tabPanel("View Data", tableOutput("data")),
        tabPanel("Clip profile", tableOutput("editableData")),
        tabPanel("Plot", plotOutput("plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Render the dataframe for the Instructions tab
  output$displayData <- renderTable({
    data
  })
  
  observeEvent(input$loadData, {
    inFile <- input$file
    if (is.null(inFile))
      return()
    
    data <- read.csv(inFile$datapath)
    updateCheckboxGroupInput(session, "cols", "Select Columns to Display:", choices = names(data))
    updateSelectInput(session, "x", "X-axis Variable", choices = names(data))
    updateSelectInput(session, "y", "Y-axis Variable", choices = names(data))
  })
  
  observeEvent(input$plot, {
    x <- input$x
    y <- input$y
    filtered_data <- subset(data, data[[x]] >= input$filter[1] & data[[x]] <= input$filter[2])
    output$plot <- renderPlot({
      ggplot(filtered_data, aes_string(x = x, y = y)) +
        geom_point() +
        labs(x = x, y = y) +
        ggtitle("Scatter Plot")
    })
  })
  
  output$data <- renderTable({
    req(data)
    data
  })
  
  output$editableData <- renderTable({
    req(data)
    datatable(data, editable = TRUE)
  })
  
  observeEvent(input$editableData_cell_edit, {
    info <- input$editableData_cell_edit
    if (!is.null(info$value)) {
      data[[info$row, info$col]] <- info$value
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
