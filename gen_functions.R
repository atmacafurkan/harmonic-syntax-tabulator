library(tidyverse)
library(magrittr)
library(data.tree)

# Use Clone() function if you want to save the version of a tree before an operation.
# For some reason R thinks assigning trees to different objects links them instead of creating a new one.

# MERGE FUNCTION, can handle internal and external merge, marks moved items and copies 
mergeMC <- function(right_arg, left_arg = NA, numeration){
  if (is.na(left_arg)){
  new_node <- Node$new(right_arg)
  field_node <- which(numeration$it == right_arg)
  new_node$Set(
    it = numeration$it[field_node],
    mc = numeration$mc[field_node],
    ac = numeration$ac[field_node],
    lb = numeration$lb[field_node],
    ft = numeration$ft[field_node],
    is_copy = numeration$is_copy[field_node],
    is_head = numeration$is_head[field_node])
  }else{
  # right_arg <- dt_trial
  # left_arg <- "DP"
  # start a new node
  new_node <- Node$new("0")
  # set attributes of the node if the input is a character
  if (is.character(left_arg)){new_node$AddChild("left_arg")
    field_left <- which(numeration$it == left_arg)
    new_node$left_arg$Set(
      it = numeration$it[field_left],
      mc = numeration$mc[field_left],
      ac = numeration$ac[field_left],
      lb = numeration$lb[field_left],
      ft = numeration$ft[field_left],
      is_copy = numeration$is_copy[field_left],
      is_head = numeration$is_head[field_left]
    )
  }else{new_node$AddChildNode(left_arg)}
  # set attributes of the node if the input is a character
  if (is.character(right_arg)){new_node$AddChild("right_arg")
    field_right <- which(numeration$it == right_arg)
    new_node$right_arg$Set(
      it = numeration$it[field_right],
      mc = numeration$mc[field_right],
      ac = numeration$ac[field_right],
      lb = numeration$lb[field_right],
      ft = numeration$ft[field_right],
      is_copy = numeration$is_copy[field_right],
      is_head = numeration$is_head[field_right]
    )
  }else{
    # check if it is internal move, turn on is_copy for the moved item and remove other attributes
    if(any(str_detect(right_arg$Get("it"), left_arg))){
      right_arg$Set(is_copy = T,
                    it="copy",
                    mc=NA,
                    ac=NA,
                    ft=NA,
                    lb="", 
                    name= "DP_copy",
                    filterFun = function(x) isLeaf(x) & any(x$Get("it") == left_arg))
      new_node$left_arg$is_moved <- T
    }
    new_node$AddChildNode(right_arg)} 
  # set attributes for the resulting merge phrase
  new_node$Set(mc = NA,
               ac = NA,
               ft = NA,
               it = "",
               is_copy = F, filterFun = isNotLeaf)
  
  # rename the nodes with their item names 
  if(is.character(left_arg)){new_node$left_arg$Set(name = new_node$left_arg$it)}
  if(is.character(right_arg)){new_node$right_arg$Set(name = new_node$right_arg$it)}
  }
  return(new_node)
  }

right_arg <- "DP"

# LABELLING FUNCTION, this is a far better labelling function that works with assigning values to the labels, far simpler. 
# It also works additively, and you can call it whenever you want.
labelMC <- function(my_tree){
  master_lb <- c("D"=1,"V"=2,"v"=3,"T"=4,"C"=5)
  x <- my_tree$Get("lb", filterFun = function(x) x$position == 1 & isNotRoot(x))
  y <- my_tree$Get("lb", filterFun = function(x) x$position == 2 & isNotRoot(x))
  z <- ifelse(x>y,x,y) %>% as.integer()
  my_tree$Set(lb=z, name=str_replace_all(paste0(names(master_lb[z]),"P"),"NAP","0"),filterFun = isNotLeaf)
  
  # add dominating domain numbers
  my_levels <- my_tree$Get("level", filterFun = isLeaf)
  my_position <- my_tree$Get("position", filterFun = isLeaf)
  for (each in 1:length(my_levels)){
    domin <- my_tree$Get("lb", filterFun = function(x) isNotLeaf(x) & x$level < my_levels[each])
    my_tree$Set(n_dominator = length(domin[!is.na(domin)]),
                filterFun = function(x) 
                  x$level == my_levels[each] &
                  x$position == my_position[each])
  }
  my_tree$Set(n_dominator = "", filterFun = function(x) x$is_copy)
  return(my_tree)
}

# AGREE FUNCTION, handles single specifiers and heads with single agreement conditions
agreeMC <- function(my_tree){
  # get agreement conditions
  goal_feats <- my_tree$Get("ac", filterFun = isLeaf)
  
  # get features for specifiers to agree with
  hp_feats <- c(tail(my_tree$Get("ft", filterFun = isLeaf),-1), "")
  
  # get features for heads to agree with
  sp_feats <- c("", head(my_tree$Get("ft", filterFun = isLeaf),-1))
  # get is_head info about leaves
  head_spec <- my_tree$Get("is_head", filterFun = isLeaf)
  
  # use a data frame to calculate agreement
  outlook <- tibble(goal_feats, hp_feats, sp_feats, head_spec)
  outlook[is.na(outlook)] <- "0"
  outlook %<>% mutate(new_goals = case_when(
    # agreement for heads with specifiers
    head_spec ~ ifelse(goal_feats == sp_feats,"", goal_feats),
    # agreement for heads
    T ~ ifelse(goal_feats == hp_feats,"", goal_feats)
  )) %>% mutate(new_goals = replace(new_goals, new_goals == "0", NA))
  # set agreement on leaves
  my_tree$Set(ac = outlook$new_goals, filterFun = isLeaf)
  
  return(my_tree)
}

