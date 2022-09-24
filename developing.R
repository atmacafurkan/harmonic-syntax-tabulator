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


# AGREE FUNCTION, agreement is carried out under sisterhood recursively
agreeMC <- function(my_tree){
  if(my_tree$leafCount>1){
    # look from left to right
  left_id <- my_tree$left_arg$Get("range_id")[1]
  right_a <- my_tree$left_arg$Get("ac")[1] %>% str_split(",") %>% unlist()
  right_f <- my_tree$right_arg$Get("ft")[1] %>% str_split(",") %>% unlist()
  if(any(right_a %in% right_f) & my_tree$Get("lb")[1] != "0"){
    right_a <- right_a[-which(right_a %in% right_f)]
    my_tree$left_arg$Set(ac = right_a, filterFun = function(x){x$position == 1 & isNotRoot(x)})
  }
  # look from right to left
  right_id <- my_tree$right_arg$Get("range_id")[1]
  left_a <- my_tree$right_arg$Get("ac")[1] %>% str_split(",") %>% unlist()
  left_f <- my_tree$left_arg$Get("ft")[1] %>% str_split(",") %>% unlist()
  # agree in the left
  if(any(left_a %in% left_f)){
    left_a <- left_a[-which(left_a %in% left_f)] %>% paste(collapse = ",")
    my_tree$left_arg$Set(ac = left_a, filterFun = function(x){x$position == 2 & isNotRoot(x)})
  }
  
  }
}


