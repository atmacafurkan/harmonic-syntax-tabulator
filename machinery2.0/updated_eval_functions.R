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
      
      # copy calculations 
      copy_dom <- n_trial$dom_counts
      violations[each, "copy"] <- copy_dom[each]*n_trial$copy[each] 
    }
    violations %<>% summarise(foc = sum(foc, na.rm = T),
                              wh = sum(wh, na.rm = T),
                              case = sum(case, na.rm = T),
                              copy = sum(copy, na.rm = T))}
  return(violations)
}

# AGREEMENT CONSTRAINT, counts features that are not valued
cons_agree <- function(my_tree){
  feat_order <- c("foc","wh","case")
  n_trial <- tibble(agree_feats = my_tree$Get("ac", filterFun = isLeaf), # extract agreement features of leafs
                    dom_counts = my_tree$Get("n_dominator", filterFun = isLeaf), # get domination counts
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

# MERGE CONDITION CONSTRAINT, recursively checks merge violations
cons_merge <- function(my_tree){


  
  
}