library(shiny)
library(DiagrammeR)

frontend <- fluidPage(
  titlePanel("Harmonic Syntax"),
  sidebarLayout(
    sidebarPanel(
      fileInput("numerationFile", label = h4("Select numeration")),
      textOutput("file_read"), # if you don't render output in the ui the server does not execute it
      numericInput("winner", label = h4("Optimal output?"), value = 100),
      
      actionButton("proceeder", label = "Next Cycle"),
      
      grVizOutput("tree"),
      
      br(),
      
      actionButton("runOptimization", label = "Calculate weights")
    ),
    
    mainPanel(
      tableOutput("costraintWeights"),
      tableOutput("eval")
    )
  )
)

backend <- function(input, output, session) {
  source("./harmonic_syntax.R")
  
  file_path <- eventReactive(input$numerationFile, {
    my_num <- import_numeration(input$numerationFile$datapath)
    numeration_name <- str_replace(input$numerationFile$name,".csv","")
    dir.create(numeration_name)
    file_last_tree <- sprintf("./%s/last_tree.rds", numeration_name)
    file_my_derivation <- sprintf("./%s/my_derivation.rds", numeration_name)
    file_last_cycle <- sprintf("./%s/last_cycle.rds", numeration_name)
    file_eval <- sprintf("./%s/my_eval.rds", numeration_name)
    saveRDS(my_num[[1]], file_last_tree)
    saveRDS(my_num[[1]], file_my_derivation)
    saveRDS(my_num[[1]], file_last_cycle)
    saveRDS(tibble(), file_eval)
  })
  output$file_read <- renderPrint({file_path()})
  
  select_tree <- eventReactive(input$proceeder,{
    numeration_name <- str_replace(input$numerationFile$name,".csv","")
    file_last_tree <- sprintf("./%s/last_tree.rds", numeration_name)
    my_tree <- readRDS(file_last_tree)
    plot_tree(my_tree)
  })
  output$tree <- renderGrViz({select_tree()})

  select_winner <- eventReactive(input$proceeder, {
    numeration_name <- str_replace(input$numerationFile$name,".csv","")
    file_last_tree <- sprintf("./%s/last_tree.rds", numeration_name)
    file_my_derivation <- sprintf("./%s/my_derivation.rds", numeration_name)
    file_last_cycle <- sprintf("./%s/last_cycle.rds", numeration_name)
    file_eval <- sprintf("./%s/my_eval.rds", numeration_name)
    
    my_outputs <- readRDS(file_last_cycle)
    
    if (input$winner < 100){
      my_tree <- my_outputs[[input$winner]]
      saveRDS(my_tree, file_last_tree)
      
      new_eval <- my_outputs %>% compose_eval() %>% mutate(input = my_tree$input, winner = rep(0, length(my_outputs)))
      new_eval$winner[input$winner] <- 1
      updated_eval <- readRDS(file_eval) %>% rbind(new_eval)
      saveRDS(updated_eval,file_eval)
      
      my_derivation <- readRDS(file_my_derivation) %>% append(my_outputs[[input$winner]])
      saveRDS(my_derivation, file_my_derivation)
    }
    
    if (input$winner == length(my_outputs)){ 
      my_eval2 <- readRDS(file_last_cycle) %>% compose_eval()
    } else {
      my_tree <- readRDS(file_last_tree)
      my_outputs <- proceed_cycle(my_tree)
      saveRDS(my_outputs, file_last_cycle)
      
      my_eval2 <- compose_eval(my_outputs) 
    }
    my_eval2
  })
  output$eval <- renderTable({select_winner()}, digits = 0, spacing = "xs")
  
  optimize_me <- eventReactive(input$runOptimization, {
    numeration_name <- str_replace(input$numerationFile$name,".csv","")
    file_eval <- sprintf("./%s/my_eval.rds", numeration_name)
    my_derivation <- readRDS(file_eval) %>% mutate(across(where(is.list), ~ map_chr(.x, as.character)))
    optimization <- weight_optimize(my_derivation, c(4:15))
    optimized_weights <- tibble(weights = optimization$par) %>% t()
    colnames(optimized_weights) <- colnames(my_derivation)[4:15]
    optimized_weights
  })
  output$costraintWeights <- renderTable({optimize_me()}, digits = 0, spacing = "xs")
}
shinyApp(ui = frontend, server = backend)
