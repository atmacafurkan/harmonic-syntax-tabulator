library(tidyverse)
library(magrittr)
library(data.tree)

source("./machinery2.0/updated_gen_functions.R")
source("./machinery2.0/updated_eval_functions.R")

df <- read.csv("basic_numeration.csv", na.strings = "NA") %>% 
  mutate(mc = ifelse(is.na(mc), "", mc))

base_der <- mergeMC("DP1","V",numeration = df) %>% labelMC()
print(base_der, "it","mc","ac","ft","lb","mc","m_vio","n_dominator", "is_copy")

output <- mergeMC("DP1","V",df) %>% labelMC() %>% mergeMC("v",df) %>% labelMC()
print(output, "it","mc","ac","ft","lb","mc","m_vio","n_dominator", "is_copy")
