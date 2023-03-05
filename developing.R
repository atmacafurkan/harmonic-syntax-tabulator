library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)
library(optimx)       # minimizing optimizer function
library(philentropy)  # KL calculating function
library(vtree)        # to export trees as png files

source("harmonic_syntax.R")

my_files <- list.files(path = "./basic_numeration/", pattern = "*.rds$", full.names = T)

df_eval <- readRDS(my_files[4])

my_trial <- weight_optimize(df_eval, c(4:15))

optimized_weights <- my_trial$par
names(optimized_weights) <- colnames(df_eval)[4:15]
optimized_weights
