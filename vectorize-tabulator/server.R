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
shinyServer(function(input, output) {
  
  # use old functions until renewed
  source("./harmonic_syntax.R")
  selected_winner <- eventReactive(input$proceed, {
    my_tree <- readRDS("my_tree.rds")
    my_outputs <- fn_cycle(my_tree)
    my_tree <- my_outputs[[input$winner]]
    saveRDS(my_tree, "my_tree.rds")
  })
  output$save <- renderPrint({selected_winner()})
  
  evaluation <- eventReactive(input$load, {
    my_tree <- readRDS("my_tree.rds")
    my_outputs <- fn_cycle(my_tree)
    my_eval <- sapply(my_outputs, function(i) i$eval) %>% t() %>% as.data.frame() %>% rownames_to_column(var = "candidate")
    my_eval
    })
  output$eval <- renderTable({evaluation()}, digits = 0, spacing = "xs")
  
  representation <- eventReactive(input$load,{
    my_tree <- readRDS("my_tree.rds")
    my_plot <- plot(as.igraph(my_tree, directed = TRUE))
    my_plot
  })
  output$tree <- renderPlot({representation()})
  
})


