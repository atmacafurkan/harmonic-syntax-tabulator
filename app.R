#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  textInput("text", "Enter text", value = "test"),
  actionButton("runScript", "Run")
)

server <- function(input, output, session) {
  
  mylist <- reactiveVal() # we will store the inputs in a reactive list
  
  observe({ # create the list
    mylist(list(
      text = input$text))
  })
  
  observeEvent(input$runScript, { # "runScript" is an action button
    source("myscript.R", local = list2env(mylist()))
  })
  
}

shinyApp(ui, server)