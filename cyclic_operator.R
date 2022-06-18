library(tidyverse)
library(magrittr)

source("gen_functions.R")
source("eval_functions.R")
source("draw_trees.R")




count_prev <- nrow(df_numeration)
# start building the tree
dt_trial <- mergeMC("DP","V")
used_args <- dt_trial$Get("it", filterFun = isLeaf) %>% as.vector()
count_prev <- nrow(df_numeration)

if (length(which(df_numeration$it %in% used_args)) != 0){
df_numeration <- df_numeration[-which(df_numeration$it %in% used_args),]}


# list all the used items with features and filter the DPs with changed values
used_items <- tibble(it = dt_trial$Get("it", filterFun = isLeaf) %>% as.vector(),
                     mc = dt_trial$Get("mc", filterFun = isLeaf) %>% as.vector(),
                     ac = dt_trial$Get("ac", filterFun = isLeaf) %>% as.vector(),
                     ft = dt_trial$Get("ft", filterFun = isLeaf) %>% as.vector(),
                     lb = dt_trial$Get("lb", filterFun = isLeaf) %>% as.vector(),
                     is_copy = dt_trial$Get("is_copy", filterFun = isLeaf) %>% as.vector(),
                     is_head =dt_trial$Get("is_head", filterFun = isLeaf) %>% as.vector()) %>%
  subset(!is_head & !is_copy)
# add copies back to the numeration with features
df_numeration %<>% rbind(used_items)

# count if any new input was used from the input
count_next <- nrow(df_numeration)

# form the evaluation
violations <- cons_profile(dt_trial) %>% mutate(exnum = NA)

# cross derivational ExNum constraint
if(count_next < count_prev){
  violations$exnum[1] <- 0 
} else {
  violations$exnum[1] <- 1
}

# generate a list of output 
single_output <- list(first = list(tree = dt_trial, eval = violations))
outputs %<>% append(single_output)


for (each in 1:nrow(df_numeration)){
  dt_new <- Clone(dt_trial) %>% mergeMC(df_numeration$it[each])
}






