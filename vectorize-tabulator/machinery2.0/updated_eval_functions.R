library(tidyverse)
library(magrittr)

# LABELLING CONSTRAINT, counts unlabelled phrases
cons_lab <- function(my_tree){
  violation <- tibble(lab=length(which(my_tree$Get("lb",filterFun = isNotLeaf) == 0)))
  return(violation)
}

# MARKEDNESS CONSTRAINT, counts agree conditions, empty agreements, and features under domination only works for leaves
cons_marked <- function(my_tree){
  violations <- tibble(mt_acs = my_tree$Get("mt_ac"), acs = my_tree$Get("ac"), feats = my_tree$Get("ft"), # get the marked feats and agreements
                       copy = my_tree$Get("is_copy"), n_doms = my_tree$Get("n_dominator")) %>% # get copy and domination counts
    mutate_at(vars(acs,feats), funs(ifelse(copy == 1,"",.))) %>% #remove feats if it is a copied element
    tidyr::separate(col = mt_acs, into = c("case_mt","foc_mt","wh_mt"), sep = "-", fill = "right") %>% # separate mt_acs into columns
    tidyr::separate(col = acs, into = c("case_a","foc_a","wh_a"), sep = "-", fill = "right") %>% # separate acs into columns
    tidyr::separate(col = feats, into = c("case","foc","wh"), sep = "-", fill = "right") %>% # separate feats into columns
    mutate_all(funs(as.numeric(as.integer(.)))) %>% replace(is.na(.),0) %>% # turn into numeric values and fill missing
    mutate_at(vars(4:9), funs(. * n_doms)) %>% dplyr::select(-n_doms) %>% # multiply each row by domination count
    summarise(case_agr = sum(case_a), foc_agr = sum(foc_a), wh_agr = sum(wh_a), # summarise agreement conditions
              case_mt = sum(case_mt), foc_mt = sum(foc_mt), wh_mt = sum(wh_mt), # summarise empty agreement
              case = sum(case),foc = sum(foc), wh = sum(wh), copy = sum(copy)) # summarise features and copy 
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
                          cons_marked(my_tree)) # marked featues returns a data frame
  return(eval_table)
}