library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)    # data trees
library(vtree)

# use old functions until renewed
source("./harmonic_syntax.R")

numerations <- list.files(path="./numerations", pattern = "*.csv", full.names = T)
my_num <- import_numeration(numerations[1])

my_tree <- Merge(my_num[[1]])[[1]] %>% Label() %>% Merge() %>% .[[1]] %>% Label() %>% Merge() %>% .[[5]]

