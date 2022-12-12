library(tidyverse)
library(magrittr)
library(philentropy)  # KL function
library(optimx)       # optimizing function
library(data.tree)

source("./machinery2.0/updated_gen_functions.R")
source("./machinery2.0/updated_eval_functions.R")

df <- read.csv("basic_numeration.csv", na.strings = "NA") %>% 
  mutate(mc = ifelse(is.na(mc), "", mc))

my_list <- list()
mergeMC("DP1","V",numeration = df) %>% labelMC() %>% mergeMC("v", df) %>% labelMC() %>% moveMC()

#my_tree <- Clone(my_list[[4]])
my_tree <- mergeMC("DP1","V",numeration = df)
my_tree %>% print("it","ft","ac","n_dominator","is_copy","mt_ac")

my_tree %<>% mt_agreeMC()
my_tree %>% print("it","ft","ac","n_dominator","is_copy","mt_ac")
