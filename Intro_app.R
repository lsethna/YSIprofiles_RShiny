## A first shiny app
#Learning the ropes!!

#housekeeping
getwd()
rm(list=ls())

#load package
librarian::shelf(readxl,shiny)

#load metadata table
metadata <- read_excel("YSIdat_metadata.xlsx")

#test
#create user interface
ui <- fluidPage(
  textInput("name", "What's your name?"),
  numericInput("age", "How old are you?", value = NA),
  textOutput("greeting")
)
#specify app behavior with server function
server <- function(input, output, session) {
  
  output$greeting <- renderText({
    paste0("Hello ", input$name, ", ", input$age, " is a great time to be alive!")
  })

}
#execute app
shinyApp(ui, server)
