library(tidyverse)
library(magrittr)
library(data.tree)

# Use Clone() function if you want to save the version of a tree before an operation.
# For some reason R thinks assigning trees to different objects links them instead of creating a new one.

# an initial numeration from which the merge operator draws futures as attributes
df_numeration <- tibble(it=c("DP", "V","v","T","C"), 
                        mc = c(NA, "DP", "VP","vP","TP"),
                        ac=c("case",NA,"wh",NA,NA),
                        ft=c("wh",NA,"case",NA,NA),
                        lb=c("D","V","v","T","C"),
                        is_copy = rep(F,5),
                        is_head = ifelse(nchar(it) ==1, T,F))

# MERGE FUNCTION, can handle internal and external merge, marks moved items and copies 
mergeMC <- function(right_arg, left_arg){
  # right_arg <- dt_trial
  # left_arg <- "DP"
  # start a new node
  new_node <- Node$new("0")
  # set attributes of the node if the input is a character
  if (is.character(left_arg)){new_node$AddChild("left_arg")
    field_left <- which(df_numeration$it == left_arg)
    new_node$left_arg$Set(
      it = df_numeration$it[field_left],
      mc = df_numeration$mc[field_left],
      ac = df_numeration$ac[field_left],
      lb = df_numeration$lb[field_left],
      ft = df_numeration$ft[field_left],
      is_copy = df_numeration$is_copy[field_left],
      is_head = df_numeration$is_head[field_left]
    )
  }else{new_node$AddChildNode(left_arg)}
  # set attributes of the node if the input is a character
  if (is.character(right_arg)){new_node$AddChild("right_arg")
    field_right <- which(df_numeration$it == right_arg)
    new_node$right_arg$Set(
      it = df_numeration$it[field_right],
      mc = df_numeration$mc[field_right],
      ac = df_numeration$ac[field_right],
      lb = df_numeration$lb[field_right],
      ft = df_numeration$ft[field_right],
      is_copy = df_numeration$is_copy[field_right],
      is_head = df_numeration$is_head[field_right]
    )
  }else{
    # check if it is internal move, turn on is_copy for the moved item and remove other attributes
    if(any(str_detect(right_arg$Get("it"), left_arg))){
      
      right_arg$Set(is_copy = T,
                    it=NA,
                    mc=NA,
                    ac=NA,
                    ft=NA,
                    lb=NA, filterFun = function(x) isLeaf(x) & any(x$Get("it") == left_arg))
      new_node$left_arg$is_moved <- T
    }
    new_node$AddChildNode(right_arg)} 
  # set attributes for the resulting merge phrase
  new_node$Set(mc = NA,
               ac = NA,
               ft = NA,
               is_copy = F, filterFun = isNotLeaf)
  
  # rename the nodes with their item names 
  if(is.character(left_arg)){new_node$left_arg$Set(name = new_node$left_arg$it)}
  if(is.character(right_arg)){new_node$right_arg$Set(name = new_node$right_arg$it)}
  return(new_node)
}

# LABELLING FUNCTION, creates the list of dominators per leaf, removes the count for moved items
labelMC <- function(my_tree){
  label_order <- c("0",NA,"D","V","v","T","C")
  if(any(class(my_tree) == "Node")){
    if(my_tree$root$name == "0"){
      my_labels <- my_tree$Get("lb") %>% as.vector() %>% unlist() %>% .[2:3]
      new_label <-ifelse(max(which(label_order %in% my_labels))>2 ,
                         label_order[max(which(label_order %in% my_labels))], "0")
      my_tree$Set(name = paste0(new_label,"P"),
                  lb = new_label, 
                  it = new_label,
                  is_head = F,
                  filterFun = isRoot)}
    # add domination information to the children once a  label is formed 
    my_tree$Set(dominated_by = paste(as.vector(my_tree$Get("dominated_by", filterFun = isLeaf)), new_label) %>% str_remove("NA "),
                filterFun = isLeaf)
    dominators <- as.vector(my_tree$Get("dominated_by", filterFun = isLeaf))
    dom_count <- integer()
    for (each in 1:length(dominators)){
      # take the number of unique labels into account
      add_count <- dominators[[each]] %>% str_split(" ") %>% unlist() %>% unique() %>% length()
      dom_count %<>% append(add_count)}
    my_tree$Set(dominator_count = dom_count, filterFun = isLeaf) 
    # remove dominator count for copies
    my_tree$Set(dominator_count = 0, filterFun = function(x) isLeaf(x) & x$is_copy)
 }
  return(my_tree)
}

# AGREE FUNCTION, only handles single agreement features per item you can agree before or after merge. Can only handle
# a single specifier for agreement. 
agreeMC <- function(my_tree){
  # get agreement conditions
  goal_feats <- my_tree$Get("ac", filterFun = isLeaf)
  
  # get features for specifiers to agree with
  hp_feats <- c(tail(my_tree$Get("ft", filterFun = isLeaf),-1), 
                   head(my_tree$Get("ft", filterFun = isLeaf),1))
  
  # get features for heads to agree with
  sp_feats <- c(tail(my_tree$Get("ft", filterFun = isLeaf),1), 
                        head(my_tree$Get("ft", filterFun = isLeaf),-1))
  # get is_head info about leaves
  head_spec <- my_tree$Get("is_head", filterFun = isLeaf)
  
  # use a data frame to calculate agreement
  outlook <- tibble(goal_feats, hp_feats, sp_feats, head_spec)
  outlook %<>% mutate(new_goals = case_when(
    # agreement for heads with specifiers
    head_spec ~ ifelse(goal_feats == sp_feats,"", goal_feats),
    # agreement for heads
    T ~ ifelse(goal_feats == hp_feats,"", goal_feats)
  ))
  # set agreement on leaves
  my_tree$Set(ac = outlook$new_goals, filterFun = isLeaf)
  
  return(my_tree)
}


dt_trial <- mergeMC("DP","V") %>% labelMC() %>%
  mergeMC("v") %>% labelMC() %>%
  mergeMC("DP") %>% labelMC() # %>% agreeMC()
my_tree <- Clone(dt_trial)

print(dt_trial, "ac","ft")
dt_trial %<>% agreeMC()
print(dt_trial, "ac","ft")



