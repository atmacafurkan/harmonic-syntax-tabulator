library(tidyverse)
library(magrittr)

# LABELLING CONSTRAINT, counts unlabelled phrases
cons_lab <- function(my_tree){
  violation <- tibble(lab=length(which(my_tree$Get("lb",filterFun = isNotLeaf) == 0)))
  return(violation)
}

# MARKEDNESS CONSTRAINT, counts features under domination
cons_marked <- function(my_tree){
  feat_order <- c("foc","wh","case")
  n_trial <- tibble(marked_feats = my_tree$Get("ft", filterFun = isLeaf), # extract features of leafs
                    dom_counts = my_tree$Get("n_dominator", filterFun= isLeaf), # get domination counts
                    copy = my_tree$Get("is_copy", filterFun = isLeaf)) %>% # get is_copy info
    mutate(marked_feats = ifelse(copy > 0, "", marked_feats)) %>% # remove features if leaf is_copy
    subset(marked_feats != "") # remove empty rows
  if (nrow(n_trial) == 0){ # if there is no markedness violation
    violations <- tibble(foc = 0, wh = 0, case = 0, copy = 0)
  }else{ # if there are markedness violations
    violations <- tibble(foc = integer(), wh = integer(), case = integer(), copy= integer())
    for (each in 1:nrow(n_trial)){ # go over each leaf, split features and calculate violation
      new_finds <- n_trial$marked_feats[each] %>% str_split(",") %>% unlist()
      feat_violations <- which(feat_order %in% new_finds)
      violations[each,feat_violations] <- as.integer(n_trial$dom_counts[each])
    }
    violations %<>% summarise(foc = sum(foc, na.rm = T),
                              wh = sum(wh, na.rm = T),
                              case = sum(case, na.rm = T),
                              copy = sum(my_tree$Get("n_dominator")*my_tree$Get("is_copy")))}
  return(violations)
}

# AGREEMENT CONSTRAINT, counts features that are not valued
cons_agree <- function(my_tree){
  feat_order <- c("foc","wh","case")
  n_trial <- tibble(agree_feats = my_tree$Get("ac", filterFun = isLeaf), # extract agreement features of leafs
                    dom_counts = my_tree$Get("n_dominator", filterFun = isLeaf), 
                    copy = my_tree$Get("is_copy", filterFun = isLeaf)) %>% # get is_copy info
    mutate(agree_feats = ifelse(copy > 0, "", agree_feats)) %>% # remove features if a leaf is_copy
    subset(agree_feats !="") # remove empty rows
  if (nrow(n_trial) == 0){ # if thre is no agree violation 
    violations <- tibble(foc_agr = 0, wh_agr = 0, case_agr = 0)
  }else{ # if there is agree violation
    violations <- tibble(foc = integer(), wh = integer(), case = integer())
    for (each in 1:nrow(n_trial)){ # go over each leaf, split features and calculate violation
      new_finds <- n_trial$agree_feats[each] %>% str_split(",") %>% unlist()
      feat_violations <- which(feat_order %in% new_finds)
      violations[each,feat_violations] <- as.integer(n_trial$dom_counts[each])
    }
    violations %<>% summarise(foc_agr = sum(foc, na.rm = T),
                              wh_agr = sum(wh, na.rm = T),
                              case_agr = sum(case, na.rm = T))}
  return(violations)
}

# MERGE CONDITION CONSTRAINT, iteratively check downwards
cons_merge <- function(my_tree){
  # check if tree has children
  violations <- 0
  if (isNotLeaf(my_tree)){
    # check left
    left_mc <- my_tree$left_arg$mc %>% str_split(",") %>% unlist() %>% as.integer()
    for_left_lb <- my_tree$right_arg$lb 
    if (any(for_left_lb == left_mc, na.rm = T)){
      NULL 
    } else if (is.na(left_mc) || is_empty(left_mc)){
      NULL
    } else {violations %<>% +1}
    
    # check right
    right_mc <- my_tree$right_arg$mc %>% str_split(",") %>% unlist() %>% as.integer()
    for_right_lb <- my_tree$left_arg$lb
    if (any(for_right_lb == right_mc, na.rm = T)){
      NULL
    } else if (is.na(right_mc) || is_empty(right_mc)){
      NULL
    } else {violations %<>% +1}
    
    # return head result by recursing
    return(violations + cons_merge(my_tree$left_arg) + cons_merge(my_tree$right_arg))
  }
  # return result for leaf node
  return(violations)
}

# EVAL FUNCTION, combines all constraint evaluations
cons_profile <- function(my_tree){
  eval_table <- bind_cols(cons_lab(my_tree), # labelling constraint returns a data frame
                          tibble(mc = cons_merge(my_tree)), # merge constraint returns a number
                          cons_agree(my_tree), # agree constraint returns a data frame
                          cons_marked(my_tree)) # marked featues returns a data frame
  return(eval_table)
}