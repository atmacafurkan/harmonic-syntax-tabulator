library(tidyverse)
library(magrittr)


source("eval_functions.R")
source("gen_functions.R")

df <- read_csv("basic_numeration.csv")

mergeMC <- function(right_arg, left_arg = NA, numeration){
  if (is.na(left_arg)){
    new_node <- Node$new(right_arg)
    field_node <- which(numeration$it == right_arg)
    new_node$Set(
      it = numeration$it[field_node],
      ml = numeration$mc_left[field_node],
      mr = numeration$mc_right[field_node],
      ac = numeration$ac[field_node],
      lb = numeration$lb[field_node],
      ft = numeration$ft[field_node],
      is_copy = numeration$is_copy[field_node],
      is_head = numeration$is_head[field_node])
  }else{
    # start a new node
    new_node <- Node$new("0")
    # set attributes of the node if the input is a character
    if (is.character(left_arg)){new_node$AddChild("left_arg")
      field_left <- which(numeration$it == left_arg)
      new_node$left_arg$Set(
        it = numeration$it[field_left],
        ml = numeration$mc_left[field_left],
        mr = numeration$mc_right[field_left],
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
        ac = numeration$ac[field_right],
        ml = numeration$mc_left[field_right],
        mr = numeration$mc_right[field_right],
        lb = numeration$lb[field_right],
        ft = numeration$ft[field_right],
        is_copy = numeration$is_copy[field_right],
        is_head = numeration$is_head[field_right]
      )
    }else{
      # check if it is internal move, turn on is_copy for the moved item and remove other attributes
      if(any(str_detect(right_arg$Get("it"), left_arg))){
        right_arg$Set(is_copy = T,
                      it = "copy",
                      ml = NA,
                      mr = NA,
                      ac = NA,
                      ft = NA,
                      lb="", 
                      name= "DP_copy",
                      filterFun = function(x) isLeaf(x) & any(x$Get("it") == left_arg))
        new_node$left_arg$is_moved <- T
      }
      new_node$AddChildNode(right_arg)} 
    # set attributes for the resulting merge phrase
    new_node$Set(ml = NA,
                 mr = NA,
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
    length(which(!is.na(numeration$mc_left))) + length(which(!is.na(numeration$mc_right)))
    
    violations <- tibble(mc = vio)

  return(violations)
}



dt_trial <- mergeMC("V","v", numeration = df) %>% 
  labelMC()

dt_trial %>% cons_merge()




