library(tidyverse)
library(magrittr)
library(data.tree)

source("./machinery2.0/updated_gen_functions.R")
source("./machinery2.0/updated_eval_functions.R")

df <- read.csv("basic_numeration.csv", na.strings = "NA") %>% 
  mutate(mc = ifelse(is.na(mc), "", mc))

base_der <- mergeMC("V",numeration = df)
print(base_der, "ac","ft","lb","mc","m_vio","n_dominator", "is_copy")

output <- mergeMC("DP1","V",df) %>% labelMC() %>% mergeMC("v",df) %>% labelMC()
plot(output)

my_list <- list()
moveMC(output)
new_ish <- Clone(my_list[[4]]) %>% labelMC() %>% mergeMC("T",df)
print(new_ish, "ac","ft","lb","mc","m_vio","n_dominator", "is_copy")
my_tree <- Clone(new_ish)

print(my_tree, "ac","ft","lb","mc","m_vio","n_dominator", "is_copy")

new_ish %>% cons_merge(df)
