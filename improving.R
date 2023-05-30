library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)
library(optimx)       # minimizing optimizer function
library(philentropy)  # KL calculating function
library(xtable)

source("harmonic_syntax.R")

dt <- import_numeration("./numerations/basic_numeration.csv")

proceed_cycle(dt[[1]]) %>% .[[2]] %>% proceed_cycle() %>% lapply(function(x) print(x,"gen","it"))
