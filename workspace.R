library(tiyverse)
library(magrittr)

source("eval_functions.R")
source("gen_functions.R")
source("draw_trees.R")
source("cyclic_operator.R")

df <- read_csv("basic_numeration.csv")

dt_trial <- mergeMC("DP1","V", df) %>% mergeMC("v", df) %>% mergeMC("DP2", df) %>% mergeMC("DP1", df) %>%
  labelMC() %>% labelMC() %>% labelMC() %>% labelMC()
print(dt_trial, "ft","ac")


dt_trial %<>% agreeMC()
print(dt_trial, "ft","ac")