library(shiny)


frontend <- fluidPage(
  # Application title
  titlePanel("Harmonic Syntax"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # Copy the line below to make a select box
      numericInput("winner", label = h3("Optimal output?"), value = 100),
      actionButton("proceed", label = "Next Cycle"),
      grVizOutput("tree"),
      br(),
      img(src = "uni_leipzig_logo_v2.svg", height = 142, width = 338),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("eval")
    )
  )
)

backend <- function(input, output) {
  # new functions for harmonic syntax
  source("./harmonic_syntax.R")
  
  # a reactive value that reads the latest tree and displays the evaluation
  selected_winner <- eventReactive(input$proceed, {
    my_tree <- readRDS("my_tree.rds")
    my_outputs <- fn_cycle(my_tree)
    if (input$winner < 100){ # if it is not the first step of the derivation, save the winner for next cycle
    my_tree <- my_outputs[[input$winner]]
    saveRDS(my_tree, "my_tree.rds")
    my_derivation <- readRDS("my_derivation.rds") %>% append(my_outputs[[input$winner]])
    saveRDS(my_derivation, "my_derivation.rds")
    }
    my_tree <- readRDS("my_tree.rds")
    my_outputs <- fn_cycle(my_tree)
    my_eval <- sapply(my_outputs, function(i) i$eval) %>% t() %>% as.data.frame() %>% rownames_to_column(var = "candidate")
    my_eval
  })
  # assign the reactive value to the output
  output$eval <- renderTable({selected_winner()}, digits = 0, spacing = "xs")
  
  
  # a reactive value that reads the latest tree and displays evaluation
  selected_tree <- eventReactive(input$proceed,{
    my_tree <- readRDS("my_tree.rds")
    fn_plotter(my_tree)
  })
  
  
  output$tree <- renderGrViz({selected_tree()}) 
  
  
}

shinyApp(ui = frontend, server = backend)





