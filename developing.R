library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)    # data trees
library(vtree)


my_files <- list.files(path = "./basic_numeration/", pattern = "*.rds$", full.names = T)

df_eval <- readRDS(my_files[4])

