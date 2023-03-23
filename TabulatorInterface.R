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
      fileInput("numerationFile", label = h4("Select numeration")),
      textOutput("file_read"), # if you don't render output in the ui the server does not execute it
      numericInput("winner", label = h4("Optimal output?"), value = 100),
      
      actionButton("proceeder", label = "Next Cycle"),
      
      grVizOutput("tree"),
      br(),
      #actionButton("remove_button", label = "Terminate Derivation")
      actionButton("runOptimization", label = "Calculate weights")
      #img(src = "uni_leipzig_logo_v2.svg", height = 142, width = 338) 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("costraintWeights"),
      tableOutput("eval")
      
    )
  )
)

# generating output
backend <- function(input, output, session) {
  # new functions for harmonic syntax
  source("./harmonic_syntax.R")
  
  # observeEvent(input$remove_button, { 
  #   removeUI(selector = "div:has(>>>> #numerationFile)") 
  #   #removeUI(selector = "div:has(> #eval)") 
  #   removeUI(selector = "div:has(> #winner)") 
  #   removeUI(selector = "#proceeder") 
  #   removeUI(selector = "#tree") 
  # }) 

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
  
  # read the numeration file
  output$file_read <- renderPrint({file_path()})

  # a reactive value that reads the latest tree and displays the evaluation
  selected_winner <- eventReactive(input$proceeder, {
    # set file paths
    numeration_name <- str_replace(input$numerationFile$name,".csv","")
    file_last_tree <- sprintf("./%s/last_tree.rds", numeration_name)
    file_my_derivation <- sprintf("./%s/my_derivation.rds", numeration_name)
    file_last_cycle <- sprintf("./%s/last_cycle.rds", numeration_name)
    file_eval <- sprintf("./%s/my_eval.rds", numeration_name)
    
    # read the last cycle
    my_outputs <- readRDS(file_last_cycle)
    
    if (input$winner < 100){ # if it is not the first step of the derivation, save the winner for next cycle
    my_tree <- my_outputs[[input$winner]]
    
    # save the eval
    new_eval <- my_outputs %>% fn_compose() %>% mutate(input = my_tree$input, winner = rep(0, length(my_outputs)))
    new_eval$winner[input$winner] <- 1
    updated_eval <- readRDS(file_eval) %>% rbind(new_eval)
    saveRDS(updated_eval,file_eval)
    
    # save winner tree from the last cycle
    saveRDS(my_tree, file_last_tree)
    
    # save derivation
    my_derivation <- readRDS(file_my_derivation) %>% append(my_outputs[[input$winner]])
    saveRDS(my_derivation, file_my_derivation)
    }
    
    if (input$winner == length(my_outputs)){ # if the winning output is a result of self merge stop advancing
      my_eval2 <- readRDS(file_last_tree)
    } else {
      # create the next cycle
      my_tree <- readRDS(file_last_tree)
      my_outputs <- fn_cycle(my_tree)
      
      # save the resulting cycle
      saveRDS(my_outputs, file_last_cycle)
      
      my_eval2 <- fn_compose(my_outputs) 
    }
    my_eval2
  })
  
  # assign the reactive value to the output
  output$eval <- renderTable({selected_winner()}, digits = 0, spacing = "xs")
  
  # a reactive value that reads the latest tree and displays evaluation
  selected_tree <- eventReactive(input$proceeder,{
    numeration_name <- str_replace(input$numerationFile$name,".csv","")
    file_last_tree <- sprintf("./%s/last_tree.rds", numeration_name)
    my_tree <- readRDS(file_last_tree)
    fn_plotter(my_tree)
  })
  
  # assign the reactive tree to the output
  output$tree <- renderGrViz({selected_tree()})
  
  optimize_me <- eventReactive(input$runOptimization, {
    numeration_name <- str_replace(input$numerationFile$name,".csv","")
    file_eval <- sprintf("./%s/my_eval.rds", numeration_name)
    my_derivation <- readRDS(file_eval) %>% mutate(across(where(is.list), ~ map_chr(.x, as.character)))
    optimization <- weight_optimize(my_derivation, c(4:15))
    optimized_weights <- tibble(weights = optimization$par) %>% t()
    colnames(optimized_weights) <- colnames(my_derivation)[4:15]
    optimized_weights
  })
  # dipslay optimized weights
  output$costraintWeights <- renderTable({optimize_me()}, digits = 0, spacing = "xs")
}
shinyApp(ui = frontend, server = backend)




