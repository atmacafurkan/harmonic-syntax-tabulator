library(tidyverse)
library(magrittr)
library(data.tree)

source("./machinery2.0/updated_gen_functions.R")
source("./machinery2.0/updated_eval_functions.R")
source("./machinery2.0/updated_cyclic_operator.R")
source("./machinery2.0/updated_draw_latex.R")
source("./machinery2.0/weight_optimizer.R")

df <- read.csv("basic_numeration.csv", na.strings = "NA") %>% 
  mutate(mc = ifelse(is.na(mc), "", mc))
dt_trial <- mergeMC("DP1","V", numeration = df)
df <- df[c(-1,-2),]

winner_output <- 100
my_derivation <- tibble()
last_tree <- list()
last_numeration <- list()
count <- 0
v_phased <- 0

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
  count <- count+1
  current_eval$cycle_number <- count
  current_eval %<>% set_winner(winner_output)
  my_derivation %<>% rbind(current_eval)
  last_tree <- my_cycle[[winner_output]]$tree
  last_numeration <- my_cycle[[winner_output]]$numeration}
}


# fix repeating inputs
my_result <- my_derivation %>% dplyr::select(-wh_agr, -foc, -case, -foc_mt, -wh_mt, -foc_agr)



