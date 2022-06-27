library(tidyverse)
library(magrittr)

source("eval_functions.R")
source("gen_functions.R")
source("draw_trees.R")
source("cyclic_operator.R")

df <- read_csv("basic_numeration.csv")

dt_trial <- mergeMC("DP1", numeration = df)

derivation <- 100

my_derivation <- tibble()

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
  current_eval %<>% set_winner(winner_output)
  my_derivation %<>% rbind(current_eval)
}


