library(shiny)
library(DiagrammeR)

# interface of the app
frontend <- fluidPage(
  # Application title
  titlePanel("Harmonic Syntax"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # Copy the line below to make a file upload manager
      fileInput("numeration_file", label = h4("Select numeration")),
      hr(),
      fluidRow(column(4, textOutput("file_read"))),
      
      numericInput("winner", label = h4("Optimal output?"), value = 100),
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

# generating output
backend <- function(input, output, session) {
  # new functions for harmonic syntax
  source("./harmonic_syntax.R")

  file_path <- eventReactive(input$numeration_file, {
    my_num <- import_numeration(input$numeration_file$datapath)
    dir.create(str_replace(input$numeration_file$name,".csv",""))
    my_file <- sprintf("./%s/last_tree.rds", str_replace(input$numeration_file$name,".csv",""))
    my_file2 <- sprintf("./%s/my_derivation.rds", str_replace(input$numeration_file$name,".csv",""))
    saveRDS(my_num[[1]], my_file)
    my_derivation <- my_num[[1]]
    saveRDS(my_derivation, my_file2)
  })
  
  # read the numeration file
  output$file_read <- renderPrint({file_path()})

  # a reactive value that reads the latest tree and displays the evaluation
  selected_winner <- eventReactive(input$proceed, {
    my_file <- sprintf("./%s/last_tree.rds",str_replace(input$numeration_file$name,".csv",""))
    my_file2 <- sprintf("./%s/my_derivation.rds",str_replace(input$numeration_file$name,".csv",""))
    my_tree <- readRDS(my_file)
    my_outputs <- fn_cycle(my_tree)
    if (input$winner < 100){ # if it is not the first step of the derivation, save the winner for next cycle
    my_tree <- my_outputs[[input$winner]]
    saveRDS(my_tree, my_file)
    my_derivation <- readRDS(my_file2) %>% append(my_outputs[[input$winner]])
    saveRDS(my_derivation, my_file2)
    }
    my_tree <- readRDS(my_file)
    my_outputs <- fn_cycle(my_tree)
    my_eval <- sapply(my_outputs, function(i) i$eval) %>% t() %>% as.data.frame() %>% rownames_to_column(var = "candidate")
    my_eval
  })
  # assign the reactive value to the output
  output$eval <- renderTable({selected_winner()}, digits = 0, spacing = "xs")
  
  # a reactive value that reads the latest tree and displays evaluation
  selected_tree <- eventReactive(input$proceed,{
    my_file <- sprintf("./%s/last_tree.rds",str_replace(input$numeration_file$name,".csv",""))
    my_tree <- readRDS(my_file)
    fn_plotter(my_tree)
  })
  
  output$tree <- renderGrViz({selected_tree()}) 
}

shinyApp(ui = frontend, server = backend)




