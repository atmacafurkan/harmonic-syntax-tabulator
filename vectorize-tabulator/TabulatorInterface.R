library(shiny)


frontend <- fluidPage(
  # Application title
  titlePanel("Harmonic Syntax"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h2("What is this?"),
      p("This is a shiny app designed to walk you through a derivation. It is a derivation of syntax in harmonic grammar."),
      h3("Defined operations"),
      p("Merge, Agree, Label"),
      # Copy the line below to make a select box
      actionButton("load", label = "Load Eval"),
      numericInput("winner", label = h3("Optimal output?"), value = 1),
      actionButton("proceed", label = "Next Cycle"),
      img(src = "uni_leipzig_logo_v2.svg", height = 142, width = 338),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h6("The evaluation table and a representational tree for the input will appear here."),
      plotOutput("tree"),
      textOutput("save"),
      tableOutput("eval")
    )
  )
)

backend <- function(input, output) {
  # new functions for harmonic syntax
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
  
}


shinyApp(ui = frontend, server = backend)





