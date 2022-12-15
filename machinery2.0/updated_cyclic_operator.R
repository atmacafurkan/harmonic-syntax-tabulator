library(tidyverse)
library(magrittr)

prepare_tableau <- function(cycle){
  resulting_tableau <- tibble()
  for (each in 1:length(cycle)){
    cand_eval <- tibble(operation = cycle[[each]]$name,
                        input = cycle[[length(cycle)]]$linear,
                        output = cycle[[each]]$linear,
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
  # check if phase occured when it is TP
   if (my_tree$name == "TP" & v_phased == 0){
    phase_out <- max(my_tree$Get("level", filterFun = function(x) x$name =="v1P")) +1
    Prune(my_tree, function(x) x$level < phase_out)
    v_phased <- 1}
  
  outputs <- list()
  # merge new items
  if (nrow(cycle_numeration) > 0){ # if there is something left in the numeration
    for (each in 1:nrow(cycle_numeration)){
      new_numeration <- cycle_numeration
      new_tree <- Clone(my_tree)
      # merge the new item
      new_tree %<>% mergeMC(new_numeration$it[each], numeration = new_numeration) # merge the new item
      # get operation name
      operation_name <- paste("merge", new_numeration$it[each])
      # remove the merged item from the numeration
      new_numeration <- cycle_numeration[-each,]
      # form the evaluation
      violations <- cons_profile(new_tree) %>% mutate(exnum = 0)
    
      # generate a list of output 
      outputs[[each]] <- list(linear = linear_tree(new_tree),
                              tree = new_tree,
                              eval = violations,
                              numeration = new_numeration,
                              name = operation_name)
      }
  }
  
  new_numeration <- cycle_numeration
  # internal merge, iterative, applies all possible movements
  new_tree <- Clone(my_tree)
  my_list <<- list()
  moveMC(new_tree)
  for(each in 1:length(my_list)){
    operation_name <- paste("move", my_list[[each]]$left_arg$it[1])
    violations <- cons_profile(my_list[[each]]) %>% mutate(exnum = ifelse(nrow(cycle_numeration > 0), 1,0))
    outputs[[each+nrow(cycle_numeration)]] <- list(linear = linear_tree(my_list[[each]]),
                                                   tree = my_list[[each]],
                                                   eval = violations,
                                                   numeration = cycle_numeration,
                                                   name = operation_name)
  }
  
  # agree and return
  new_tree <- Clone(my_tree) %>% agreeMC()
  new_tree2 <- Clone(my_tree) 
  ac_same <- all(new_tree$Get("ac") == new_tree2$Get("ac"))
  if (new_tree$count != 0 & !ac_same){
    # form the evaluation
    # agreeing doesnt count towards exnum
    violations <- cons_profile(new_tree) %>% mutate(exnum = 0)
    outputs[[length(outputs)+1]] <- list(linear = linear_tree(new_tree),
                                         tree = new_tree,
                                         eval = violations,
                                         numeration = cycle_numeration,
                                         name = "agree")}
  
  # empty agree and return
  new_tree <- Clone(my_tree) %>% mt_agreeMC()
  new_tree2 <- Clone(my_tree) 
  ac_same <- all(new_tree$Get("ac") == new_tree2$Get("ac"))
  if (new_tree$count != 0 & !ac_same){
    # form the evaluation
    # agreeing doesnt count towards exnum
    violations <- cons_profile(new_tree) %>% mutate(exnum = 0)
    outputs[[length(outputs)+1]] <- list(linear = linear_tree(new_tree),
                                         tree = new_tree,
                                         eval = violations,
                                         numeration = cycle_numeration,
                                         name = "empty_agree")}
  
  # label and return
  new_tree <- Clone(my_tree) %>% labelMC()
  new_tree2 <- Clone(my_tree) 
  lb_same <- all(new_tree$Get("lb") == new_tree2$Get("lb"))
  if (new_tree$count != 0 & !lb_same){
    # form the evaluation
    # labelling doesnt count towards exnum   
    violations <- cons_profile(new_tree) %>% mutate(exnum = 0)
    outputs[[length(outputs)+1]] <- list(linear = linear_tree(new_tree),
                                         tree = new_tree,
                                         eval = violations,
                                         numeration = cycle_numeration,
                                         name = "label")}
  
  # return the tree itself, actually it is merging with nothing, which violates exnum
  new_tree <- Clone(my_tree)
  # form the evaluation
  violations <- cons_profile(new_tree) %>% mutate(exnum = ifelse(nrow(cycle_numeration > 0), 1,0))

  # check if there is anything left in the numeration
  if (nrow(new_numeration) == 0){violations$exnum[1] <- 0}
  outputs[[length(outputs)+1]] <- list(linear = linear_tree(new_tree),
                                       tree = new_tree,
                                       eval = violations,
                                       numeration = cycle_numeration,
                                       name = "self")
  return(outputs)
}
