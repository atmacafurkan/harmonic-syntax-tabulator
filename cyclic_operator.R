library(tidyverse)
library(magrittr)

source("gen_functions.R")
source("eval_functions.R")
source("draw_trees.R")

df_numeration <- readRDS("basic_numeration.rds") 


dt_trial <- mergeMC("DP", numeration = df_numeration)


# you feed the step an initial tree and a numeration to use it with
# it returns a list of all possible Merge operations together with an Agree and Label operations, it returns itself last.
cycle_step <- function(my_tree, cycle_numeration){
  
outputs <- list()

for (each in 1:nrow(cycle_numeration)){
new_numeration <- cycle_numeration
new_tree <- Clone(my_tree)

# number of distinct elements in the tree before merge
count_prev <- length(new_tree$Get("it", filterFun = function(x) isLeaf(x) & !x$is_copy))

new_tree %<>% mergeMC(new_numeration$it[each], numeration = new_numeration)

used_args <- new_tree$Get("it", filterFun = function(x) isLeaf(x) & !x$is_copy) %>% as.vector()
if (length(which(new_numeration$it %in% used_args)) != 0){
new_numeration <- new_numeration[-which(new_numeration$it %in% used_args),]}

# list all the used items with features and filter the DPs with changed values
used_items <- tibble(it = new_tree$Get("it", filterFun = isLeaf) %>% as.vector(),
                     mc = new_tree$Get("mc", filterFun = isLeaf) %>% as.vector(),
                     ac = new_tree$Get("ac", filterFun = isLeaf) %>% as.vector(),
                     ft = new_tree$Get("ft", filterFun = isLeaf) %>% as.vector(),
                     lb = new_tree$Get("lb", filterFun = isLeaf) %>% as.vector(),
                     is_copy = new_tree$Get("is_copy", filterFun = isLeaf) %>% as.vector(),
                     is_head = new_tree$Get("is_head", filterFun = isLeaf) %>% as.vector()) %>%
  subset(!is_head & !is_copy)
# add copies back to the numeration with features
new_numeration %<>% rbind(used_items)

# number of distinct elements in the tree after merge
count_next <- length(new_tree$Get("it", filterFun = function(x) isLeaf(x) & !x$is_copy))

# form the evaluation
violations <- cons_profile(new_tree) %>% mutate(exnum = NA)

# cross derivational ExNum constraint
if(count_prev < count_next){
  violations$exnum[1] <- 0 
} else {
  violations$exnum[1] <- 1
}

# generate a list of output 
outputs[[each]] <- list(linear = linear_tree(new_tree),
                        tree_latex = latex_tree(new_tree),
                        tree_linear_latex = latex_linear_tree(new_tree),
                        tree = new_tree,
                        eval = violations,
                        numeration = new_numeration)

}

# return the tree itself
new_tree <- Clone(my_tree)
# form the evaluation
violations <- cons_profile(new_tree) %>% mutate(exnum = 1)
outputs[[length(outputs)+1]] <- list(linear = linear_tree(new_tree),
                                            tree_latex = latex_tree(new_tree),
                                            tree_linear_latex = latex_linear_tree(new_tree),
                                            tree = new_tree,
                                            eval = violations,
                                            numeration = cycle_numeration)

# agree and return
new_tree <- Clone(my_tree) %>% agreeMC()
new_tree2 <- Clone(my_tree) 
is_different <- any(new_tree$Get("ac") != new_tree2$Get("ac"), na.rm = T)
if (new_tree$count != 0 & is_different){
# form the evaluation
violations <- cons_profile(new_tree) %>% mutate(exnum = 1)
outputs[[length(outputs)+1]] <- list(linear = linear_tree(new_tree),
                                            tree_latex = latex_tree(new_tree),
                                            tree_linear_latex = latex_linear_tree(new_tree),
                                            tree = new_tree,
                                            eval = violations,
                                            numeration = cycle_numeration)}

# label and return
new_tree <- Clone(my_tree) %>% labelMC()
new_tree2 <- Clone(my_tree) 
is_different <- any(new_tree$Get("name") != new_tree2$Get("name"), na.rm = T)
if (new_tree$count != 0 & is_different){
# form the evaluation
violations <- cons_profile(new_tree) %>% mutate(exnum = 1)
outputs[[length(outputs)+1]] <- list(linear = linear_tree(new_tree),
                                            tree_latex = latex_tree(new_tree),
                                            tree_linear_latex = latex_linear_tree(new_tree),
                                            tree = new_tree,
                                            eval = violations,
                                            numeration = cycle_numeration)}
return(outputs)
}


cycle_2 <- cycle_step(dt_trial, df_numeration)
cycle_2_winner <- 2

cycle_3 <- cycle_step(cycle_2[[cycle_2_winner]]$tree, cycle_2[[cycle_2_winner]]$numeration)
cycle_3_winner <- 6


cycle_4 <- cycle_step(cycle_3[[cycle_3_winner]]$tree, cycle_3[[cycle_3_winner]]$numeration)
