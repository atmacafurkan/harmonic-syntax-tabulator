library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)    # data trees
library(vtree)

# use old functions until renewed
source("./harmonic_syntax.R")

numerations <- list.files(path="./numerations", pattern = "*.csv", full.names = T)
my_num <- import_numeration(numerations[1])

saveRDS(my_num[[1]], "my_tree.rds")
my_derivation <- my_num[[1]]
saveRDS(my_derivation, "my_derivation.rds")
