library(tidyverse)
library(magrittr)


# LABELLING CONSTRAINT, counts unlabelled phrases
cons_lab <- function(my_tree){
  violation <- tibble(lab=length(is.na(my_tree$Get("lb",filterFun = isNotLeaf)) %>% .[. ==T]))
  return(violation)
}

# MARKEDNESS CONSTRAINT, counts features under domination
cons_marked <- function(my_tree){
  feat_order <- c("foc","wh","case")
  n_trial <- tibble(marked_feats = my_tree$Get("ft", filterFun = isLeaf),
                    dom_counts = my_tree$Get("n_dominator", filterFun= isLeaf)) %>% drop_na()
  if (nrow(n_trial) == 0){
    violations <- tibble(foc = 0, wh = 0, case = 0)
  }else{
  violations <- tibble(foc = integer(), wh = integer(), case = integer())
  for (each in 1:nrow(n_trial)){
    new_finds <- n_trial$marked_feats[each] %>% str_split(",") %>% unlist()
    feat_violations <- which(feat_order %in% new_finds)
    violations[each,feat_violations] <- n_trial$dom_counts[each]
  }
  violations %<>% summarise(foc = sum(foc, na.rm = T),
                            wh = sum(wh, na.rm = T),
                            case = sum(case, na.rm = T))}
  return(violations)
}

# AGREEMENT CONSTRAINT, counts features that are not valued
cons_agree <- function(my_tree){
  feat_order <- c("foc","wh","case")
  n_trial <- tibble(marked_feats = my_tree$Get("ac", filterFun = isLeaf),
                    dom_counts = my_tree$Get("n_dominator", filterFun= isLeaf)) %>% drop_na()
  if (nrow(n_trial) == 0){
    violations <- tibble(foc_agr = 0, wh_agr = 0, case_agr = 0)
  }else{
  violations <- tibble(foc = integer(), wh = integer(), case = integer())
  for (each in 1:nrow(n_trial)){
    new_finds <- n_trial$marked_feats[each] %>% str_split(",") %>% unlist()
    feat_violations <- which(feat_order %in% new_finds)
    violations[each,feat_violations] <- n_trial$dom_counts[each]
  }
  violations %<>% summarise(foc_agr = sum(foc, na.rm = T),
                            wh_agr = sum(wh, na.rm = T),
                            case_agr = sum(case, na.rm = T))}
  return(violations)
}

# MERGE CONDITION CONSTRAINT, count the number of merge conditions in the structure
# MERGE CONDITION CONSTRAINT, count the number of remaining merge conditions in the structure
cons_merge <- function(my_tree,numeration){
  # check complement merge conditions
  subcat_r <- my_tree$Get("mr", filterFun = function(x) x$position == 1 & isNotRoot(x))
  merging_r <- my_tree$Get("lb", filterFun = function(x) x$position == 2)
  subcat_r[which(merging_r == subcat_r)] <- NA
  my_tree$Set(mr = subcat_r, filterFun = function(x) x$position == 1 & isNotRoot(x))
  
  # check specifier merge conditions  
  subcat_l <- my_tree$Get("ml", filterFun = function(x) x$position == 1 & isNotRoot(x))
  to_check_l <- subcat_l %>% is.na() %>% !.
  merging_l <- my_tree$Get("lb", filterFun = function(x) x$position == 1 & isNotRoot(x)) %>% 
    append(NA,.) %>% head(-1)
  subcat_l[which(merging_l == subcat_l)] <- NA
  my_tree$Set(ml = subcat_l, filterFun = function(x) x$position == 1 & isNotRoot(x))
  
  # combine number of violations  
  vio <- length(which(!is.na(my_tree$Get("mr", filterFun = isLeaf)))) + 
    length(which(!is.na(my_tree$Get("ml", filterFun = isLeaf))))
  remaining_mc <- length(which(!is.na(numeration$mc_left))) + length(which(!is.na(numeration$mc_right)))
  
  violations <- tibble(mc = vio + remaining_mc)
  
  return(violations)
}

# EVAL FUNCTION, combines all constraint evaluations
cons_profile <- function(my_tree, numeration){
eval_table <- bind_cols(cons_lab(my_tree), cons_merge(my_tree, numeration), 
                        cons_agree(my_tree), cons_marked(my_tree))
return(eval_table)
}
