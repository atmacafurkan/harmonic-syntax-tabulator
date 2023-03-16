library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)
library(optimx)       # minimizing optimizer function
library(philentropy)  # KL calculating function
library(vtree)        # to export trees as png files

source("harmonic_syntax.R")

my_files <- list.files(path = "./basic_numeration/", pattern = "*.rds$", full.names = T)

df_eval <- readRDS(my_files[4]) %>% mutate(across(where(is.list), ~ map_chr(.x, as.character)))

my_optimization <- weight_optimize(df_eval, c(4:15))

optimized_weights <- my_optimization$par
names(optimized_weights) <- colnames(df_eval)[4:15]
optimized_weights

trial <- df_eval[,4:15] %>% as.data.frame() %>% mutate_all(as.integer) %>% data.matrix()

df_eval %<>% mutate(harmonies = as.numeric(trial %*% optimized_weights))
rm(trial)

