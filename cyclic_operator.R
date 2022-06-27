library(tidyverse)
library(magrittr)

prepare_tableau <- function(cycle){
  resulting_tableau <- tibble()
  for (each in 1:length(cycle)){
  cand_eval <- tibble(input = cycle[[length(cycle)]]$tree_latex,
                      output = cycle[[each]]$tree_latex,
                      freq = 0) %>% cbind(cycle[[each]]$eval)  
  resulting_tableau %<>% rbind(cand_eval)
  }
  return(resulting_tableau)
}

set_winner <- function(df, winner){
  df$freq[winner] <- 1
  return(df)
}

# you feed the step an initial tree and a numeration to use it with
# it returns a list of all possible Merge operations together with an Agree and Label operations, it returns itself last.
# each cycle is a list that contains all relevant information for all the possible outputs given the input
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
                     mc_left = new_tree$Get("ml", filterFun = isLeaf) %>% as.vector(),
                     mc_right = new_tree$Get("mr", filterFun = isLeaf) %>% as.vector(),
                     ac = new_tree$Get("ac", filterFun = isLeaf) %>% as.vector(),
                     ft = new_tree$Get("ft", filterFun = isLeaf) %>% as.vector(),
                     lb = new_tree$Get("lb", filterFun = isLeaf) %>% as.vector(),
                     is_copy = new_tree$Get("is_copy", filterFun = isLeaf) %>% as.vector(),
                     is_head = new_tree$Get("is_head", filterFun = isLeaf) %>% as.vector()) %>%
  subset(!is_head & !is_copy)


# number of distinct elements in the tree after merge
count_next <- length(new_tree$Get("it", filterFun = function(x) isLeaf(x) & !x$is_copy))

# form the evaluation
violations <- cons_profile(new_tree, new_numeration) %>% mutate(exnum = NA)

# cross derivational ExNum constraint
if(count_prev < count_next){
  violations$exnum[1] <- 0 
} else {
  violations$exnum[1] <- 1
}


# add copies back to the numeration with updated features if any
new_numeration %<>% rbind(used_items)


# generate a list of output 
outputs[[each]] <- list(linear = linear_tree(new_tree),
                        tree_latex = latex_tree(new_tree),
                        tree_linear_latex = latex_linear_tree(new_tree),
                        tree = new_tree,
                        eval = violations,
                        numeration = new_numeration)

}
# agree and return
new_tree <- Clone(my_tree) %>% agreeMC()
new_tree2 <- Clone(my_tree) 
is_different <- any(new_tree$Get("ac") != new_tree2$Get("ac"), na.rm = T)
if (new_tree$count != 0 & is_different){
# form the evaluation
violations <- cons_profile(new_tree, new_numeration) %>% mutate(exnum = 1)
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
violations <- cons_profile(new_tree, new_numeration) %>% mutate(exnum = 1)
outputs[[length(outputs)+1]] <- list(linear = linear_tree(new_tree),
                                            tree_latex = latex_tree(new_tree),
                                            tree_linear_latex = latex_linear_tree(new_tree),
                                            tree = new_tree,
                                            eval = violations,
                                            numeration = cycle_numeration)}
# return the tree itself
new_tree <- Clone(my_tree)
# form the evaluation
violations <- cons_profile(new_tree, new_numeration) %>% mutate(exnum = 1)
outputs[[length(outputs)+1]] <- list(linear = linear_tree(new_tree),
                                     tree_latex = latex_tree(new_tree),
                                     tree_linear_latex = latex_linear_tree(new_tree),
                                     tree = new_tree,
                                     eval = violations,
                                     numeration = cycle_numeration)
return(outputs)
}

