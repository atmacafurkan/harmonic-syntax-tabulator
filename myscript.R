library(tidyverse)
library(magrittr)
source("gen_functions.R")
source("eval_functions.R")
source("draw_trees.R")


writeLines(mergeMC(text1,text2) %>% linear_tree(), "output.txt")

mergeMC(text1, text2) %>% cons_profile() %>% saveRDS("trial_evaluated.rds")