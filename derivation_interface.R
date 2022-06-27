library(tidyverse)
library(magrittr)

source("eval_functions.R")
source("gen_functions.R")
source("draw_trees.R")
source("cyclic_operator.R")
source("depricated_functions.R")


df <- read_csv("basic_numeration.csv")

dt_trial <- mergeMC("DP1", numeration = df) 

winner_output <- 100

my_derivation <- tibble()
last_tree <- list()
last_numeration <- list()

while(winner_output != 0){
if (winner_output == 100){
  my_cycle <- cycle_step(dt_trial, df)
  }else{
  my_cycle <- cycle_step(my_cycle[[winner_output]]$tree, my_cycle[[winner_output]]$numeration)
  }
  current_eval <- prepare_tableau(my_cycle) 
  View(current_eval)
  winner_output <- readline(prompt = "Winner output: ");
  winner_output %<>% as.integer()
  if(winner_output !=0){
  current_eval %<>% set_winner(winner_output)
  my_derivation %<>% rbind(current_eval)
  last_tree <- my_cycle[[winner_output]]$tree
  last_numeration <- my_cycle[[winner_output]]$numeration}
}


# fix repeating inputs
my_derivation %<>% dplyr::select(-wh_agr, -foc_agr, -foc, -case)
my_derivation$input[which(duplicated(my_derivation$input))] <- c("")

my_derivation <- rbind(colnames(my_derivation), my_derivation) %>% rbind(rep("",length(my_derivation)))
# fix colnames
colnames(my_derivation) <- c(rep("",3), tail(colnames(my_derivation),-3))

write_tsv(my_derivation, "./MaxentGrammarTool/trial_derivation2.txt")

