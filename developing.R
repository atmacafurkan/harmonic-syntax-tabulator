library(tidyverse)
library(magrittr)
library(data.tree)

source("./updated_gen_functions.R")

df <- read_csv("basic_numeration.csv")
output <- mergeMC("DP1","V",df) %>% labelMC() %>% mergeMC("v",df) %>% labelMC()
plot(output)
print(output,"it","n_dominator")


my_list <- list()
moveMC(output)
my_tree <- Clone(my_list[[4]]) %>% labelMC()
print(my_tree,"it","lb","ft","ac","is_copy", "range_id","n_dominator")
plot(my_tree)



my_tree %>% agreeMC() %>% print("it","lb","ft","ac","is_copy", "range_id","n_dominator")

print(my_tree,"it","lb","ft","ac","is_copy", "range_id","n_dominator")
