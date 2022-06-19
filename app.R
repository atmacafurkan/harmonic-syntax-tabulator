#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(magrittr)

df_numeration <- readRDS("basic_numeration.rds")

ui <- fluidPage(
  textInput("text1", "First head", value = ""),
  textInput("text2", "First phrase", value = ""),
  actionButton("runScript", "Run"),
  actionButton("my_button", "Add UI")
  
)

server <- function(input, output, session) {
  # an initial numeration from which the merge operator draws futures as attributes

  mylist <- reactiveVal() # we will store the inputs in a reactive list
  
  observe({ # create the list
    mylist(list(
      text1 = input$text1,
      text2 = input$text2))
  })
  
  observeEvent(input$my_button, {
      removeUI(
        selector = "div:has(> #text2)"
      )
    insertUI(
      selector = "#my_button",
      where = "afterEnd",
      ui = textInput(paste0("txt", input$my_button),
                     "Insert some text")
    )
  })
  
  observeEvent(input$runScript, { # "runScript" is an action button
    source("myscript.R", local = list2env(mylist()))
  })
  
}


shinyApp(ui, server)

