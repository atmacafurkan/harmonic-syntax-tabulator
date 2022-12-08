library(tidyverse)
library(magrittr)
library(philentropy)  # KL function
library(optimx)       # optimizing function
library(data.tree)

source("./machinery2.0/updated_gen_functions.R")
source("./machinery2.0/updated_eval_functions.R")
source("./machinery2.0/updated_draw_latex.R")
source("./machinery2.0/weight_optimizer.R")

df <- read.csv("basic_numeration.csv", na.strings = "NA") %>% 
  mutate(mc = ifelse(is.na(mc), "", mc))

my_list <- list()
mergeMC("DP1","V",numeration = df) %>% labelMC() %>% mergeMC("v", df) %>% labelMC() %>% moveMC()

my_tree <- Clone(my_list[[3]]) 

print(my_tree, "it","lb","mc","m_vio","is_copy")

my_tree %>% cons_merge()

