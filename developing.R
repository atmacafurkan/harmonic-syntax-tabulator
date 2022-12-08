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

my_tree <- Clone(my_list[[4]]) %>% agreeMC() %>% labelMC()
my_list <- list()


print(my_tree, "it","lb","mc","is_copy")
moveMC(my_tree$right_arg, my_tree)
print(my_list[[2]], "it","is_copy", "range_id", "keep_me", "n_dominator")
my_list <- list()
moveMC(my_tree)

my_list[[4]] %>% print("it","lb","is_copy", "n_dominator", "range_id", "keep_me")

