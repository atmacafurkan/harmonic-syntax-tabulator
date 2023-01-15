library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)

# a function to read a data frame into a list of tree nodes
import_numeration <- function(path_to_numeration){
  # read numeration from csv
  df <- read_csv(path_to_numeration) %>%
    mutate(mc = ifelse(is.na(mc), "", mc), # fix na values for mc
           pathString = "new_arg") # add pathstring value for as.Node function
  
  # turn data frame into a list of data.trees
  numeration_list <- sapply(split(df, 1:nrow(df)), as.Node, USE.NAMES = F)
  
  # return the list of trees
  return(numeration_list)
}


# GEN FUNCTIONS #####
# recurse function to update domination counts
recurse <- function(active_tree, output_tree = active_tree){ 
  # get n_dominators
  n_doms <- output_tree$Get("n_dominator")
  # create a new
  my_tree <- Clone(active_tree)
  
  # learn mother and children labels
  mother_lab <- my_tree$lb
  left_lab <- my_tree$left_arg$lb
  right_lab <- my_tree$right_arg$lb
  
  # update left
  if (mother_lab > left_lab){
    # increase global domination
    range_ids <- my_tree$left_arg$Get("range_id")
    n_doms[range_ids] <- my_tree$left_arg$Get("n_dominator")+1
    # increase local domination
    my_tree$left_arg$Set(n_dominator = my_tree$left_arg$Get("n_dominator")+1)
    output_tree$Set(n_dominator = n_doms)
  }
  
  # update right
  if (mother_lab > right_lab){
    # increase global domination
    range_ids <- my_tree$right_arg$Get("range_id")
    n_doms[range_ids] <- my_tree$right_arg$Get("n_dominator")+1
    # increase local domination
    my_tree$right_arg$Set(n_dominator = my_tree$right_arg$Get("n_dominator")+1)
    output_tree$Set(n_dominator = n_doms)
  }
  
  # recursively call the function on the left child
  if (is.null(my_tree$left_arg)){} else if (my_tree$left_arg$leafCount > 1){ # check if there is left_arg, if more than 1
    recurse(my_tree$left_arg,output_tree)
  }
  
  # recursively call the function on the right child
  if (is.null(my_tree$right_arg)){} else if (my_tree$right_arg$leafCount > 1){ # check if there is right_arg, if more than 1
    recurse(my_tree$right_arg,output_tree)
  }
  return(output_tree)
}

# merge function, only takes trees as arguments
Merge <- function(my_right, my_left){
  # clone tree nodes to avoid entanglement
  left_arg <- Clone(my_left)
  right_arg <- Clone(my_right)
  
  # name the root nodes of arguments
  left_arg$Set(name = "left_arg", filterFun = isRoot)
  right_arg$Set(name = "right_arg", filterFun = isRoot)
  
  # create new node and set default values
  new_node <- Node$new("0", mc = "", ac = "", ft = "", lb = 0, it = 0, is_copy = F) 
  
  # add children first left then right as sibling
  new_node$AddChildNode(left_arg)$AddSiblingNode(right_arg)
  
  # mark internal and external merge
  new_node$exnum <- ifelse(left_arg$range_id == -1, 0, 1)
  
  # add range ids for unique identification and default domination counts
  new_node$Set(range_id = 1:length(new_node$Get("lb")))
  
  # renew domination counts
  new_node$Set(n_dominator = 0)
  new_node %<>% recurse()
  return(new_node)
}

# label function, does not check if there is a label
Label <- function(input_tree){
  # clone to remove connections
  my_tree <- Clone(input_tree)
  
  # carry up features
  if (my_tree$right_arg$lb >= my_tree$left_arg$lb){
    # gather the values to be passed up
    new_values <- list(lb = my_tree$right_arg$lb,
                       ft = my_tree$right_arg$ft,
                       ac = my_tree$right_arg$ac,
                       it = my_tree$right_arg$it)
    # set the new values in the labelled phrase
    rlang::exec(my_tree$Set, !!!new_values, filterFun = isRoot)
    # remove the moved values
    my_tree$right_arg$ft <- ""
    my_tree$right_arg$ac <- ""
  } else {
    # gather the values to be passed up
    new_values <- list(lb = my_tree$left_arg$lb,
                       ft = my_tree$left_arg$ft,
                       ac = my_tree$left_arg$ac,
                       it = my_tree$left_arg$it)
    # set the new values in the labelled phrase
    rlang::exec(my_tree$Set, !!!new_values, filterFun = isRoot)
    # remove the moved values
    my_tree$left_arg$ft <- ""
    my_tree$left_arg$ac <- ""
  }
  # renew domination counts
  my_tree$Set(n_dominator = 0)
  my_tree %<>% recurse()
  return(my_tree)
}

# agree function, compare ft and ac features of the last siblings
Agree <- function(input_tree){
  # clone tree to remove connections
  my_tree <- Clone(input_tree)
  
  # agree left
  left_ac <- unlist(str_split(my_tree$left_arg$ac, "-"))
  lefter_ft <- unlist(str_split(my_tree$right_arg$ft, "-"))
  
  if (any(left_ac == lefter_ft)){ # sometimes ac is completely empty, check if there is any match
    match_number <- which(left_ac == lefter_ft)
    left_ac[match_number] <- "0" # turn matches into 0 in ac
    lefter_ft[match_number] <- "0" # turn matches into 0 in feats
    my_tree$left_arg$ac <- paste(left_ac, collapse = "-")
    my_tree$right_arg$ft <- paste(lefter_ft, collapse="-")
  }
  
  # agree right
  right_ac <- unlist(str_split(my_tree$right_arg$ac, "-"))
  righter_ft <- unlist(str_split(my_tree$left_arg$ft, "-"))
  
  if (any(right_ac == righter_ft)){ # sometimes ac is completely empty, check if there is any match
    match_number <- which(right_ac == righter_ft)
    right_ac[match_number] <- "0"
    righter_ft[match_number] <- "0"
    my_tree$right_arg$ac <- paste(right_ac,collapse = "-")
    my_tree$left_arg$ft <- paste(righter_ft, collapse = "-")
  }
  return(my_tree)
}


# EVAL FUNCTIONS #####
# merge constraint, iteratively check downwards
merge_cons <- function(my_tree){
  violations <- 0
  # check if tree has children
  if (isNotLeaf(my_tree)){
    # check left
    left_mc <- as.integer(unlist(str_split(my_tree$left_arg$mc,",")))
    for_left_lb <- my_tree$right_arg$lb 
    if (any(for_left_lb == left_mc, na.rm = T)){
      NULL 
    } else if (is.na(left_mc) || is_empty(left_mc)){
      NULL
    } else {violations %<>% +1}
    
    # check right
    right_mc <-  as.integer(unlist(str_split(my_tree$right_arg$mc,",")))
    for_right_lb <- my_tree$left_arg$lb
    if (any(for_right_lb == right_mc, na.rm = T)){
      NULL
    } else if (is.na(right_mc) || is_empty(right_mc)){
      NULL
    } else {violations %<>% +1}
    
    # return head result by recursing
    return(violations + merge_cons(my_tree$left_arg) + merge_cons(my_tree$right_arg))
  }
  # return result for leaf node
  return(violations)
}







# DERIVATION INTERFACE ####




