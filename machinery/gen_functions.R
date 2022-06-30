library(tidyverse)
library(magrittr)
library(data.tree)

# Use Clone() function if you want to save the version of a tree before an operation.
# For some reason R thinks assigning trees to different objects links them instead of creating a new one.


# MERGE FUNCTION, can handle internal and external merge, marks moved items and copies and carries Agree and features up
mergeMC <- function(right_arg, left_arg = NA, numeration){
  if (is.na(left_arg)){
    new_node <- Node$new(right_arg)
    field_node <- which(numeration$it == right_arg)
    new_node$Set(
      it = numeration$it[field_node],
      mc = numeration$mc[field_node],
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
        mc = numeration$mc[field_left],
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
        mc = numeration$mc[field_right],
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
                      mc = NA,
                      ml = NA,
                      mr = NA,
                      ac = 0,
                      ft = 0,
                      name= "DPc",
                      filterFun = function(x) isLeaf(x) & any(x$Get("it") == left_arg))
        new_node$left_arg$is_moved <- T
      }
      new_node$AddChildNode(right_arg)} 
    new_node$Set(mc = NA, ml = NA, mr = NA, ac = NA, ft = NA, lb= NA, it = "", is_copy = F, filterFun = isRoot)
    # # carry over the Agree condition and features up the phrase.
    # if(new_node$left_arg$is_head){
    #   # set attributes for the resulting merge phrase
    #   new_node$Set(ml = NA,
    #                mr = NA,
    #                ac = new_node$left_arg$Get("ac", filterFun = function(x) isLeaf(x) & x$is_head),
    #                ft = new_node$left_arg$Get("ft", filterFun = function(x) isLeaf(x) & x$is_head),
    #                it = "",
    #                is_copy = F, filterFun = isRoot)
    #   new_node$left_arg$Set(ac = 0, ft = 0, filterFun = function(x) isLeaf(x) & x$is_head)
    # } else {
    #   # set attributes for the resulting merge phrase
    #   new_node$Set(ml = NA,
    #                mr = NA,
    #                ac = new_node$children[[2]]$Get("ac", filterFun = isNotLeaf)[1],
    #                ft = new_node$children[[2]]$Get("ft", filterFun = isNotLeaf)[1],
    #                it = "",
    #                is_copy = F, filterFun = isRoot)
    #   new_node$children[[2]]$ac <- 0
    #   new_node$children[[2]]$ft <- 0
    # }
x <- ifelse(is.na(new_node$children[[1]]$mc) || is_empty(new_node$children[[1]]$mc),"A", new_node$children[[1]]$mc)
y <- ifelse(is.na(new_node$children[[2]]$lb) || is_empty(new_node$children[[2]]$lb),"B", new_node$children[[2]]$lb)

if(x ==y){
  new_node$children[[1]]$mc <- NA
}
    # rename the nodes with their item names 
    if(is.character(left_arg)){new_node$left_arg$Set(name = new_node$left_arg$it)}
    if(is.character(right_arg)){new_node$right_arg$Set(name = new_node$right_arg$it)}
  }
  return(new_node)
}

# LABELLING FUNCTION, this is a far better labelling function that works with assigning values to the labels, far simpler. 
# It also works additively, and you can call it whenever you want.
labelMC <- function(my_tree){
  if (!is.na(my_tree$lb)){
    return(my_tree)
  } else {
  #master_lb <- c("D"=1,"V"=2,"v"=3,"T"=4,"C"=5)
  master_lb <- c("D"=1,"V"=2,"v1"=3,"v2"=4,"T"=5,"C"=6)
  x <- my_tree$Get("lb", filterFun = function(x) x$position == 1 & isNotRoot(x))
  y <- my_tree$Get("lb", filterFun = function(x) x$position == 2 & isNotRoot(x))
  z <- ifelse(x>y,x,y) %>% as.integer()
  my_tree$Set(lb=z, name=str_replace_all(paste0(names(master_lb[z]),"P"),"NAP","0"),filterFun = isNotLeaf)
  
  # add dominating domain numbers
  my_levels <- my_tree$Get("level", filterFun = isLeaf)
  my_position <- my_tree$Get("position", filterFun = isLeaf)
  for (each in 1:length(my_levels)){
    domin <- my_tree$Get("lb", filterFun = function(x) isNotLeaf(x) & x$level < my_levels[each]) %>% unique()
    my_tree$Set(n_dominator = length(domin[!is.na(domin)]),
                filterFun = function(x) 
                  x$level == my_levels[each] &
                  x$position == my_position[each])
  }
  #my_tree$Set(n_dominator = "", filterFun = function(x) x$is_copy)
  if (ifelse(length(my_tree$children[[1]]$is_head)==0,F,my_tree$children[[1]]$is_head)){
    my_tree$Set(mc = NA,
                ml = NA,
                mr = NA,
                ac = my_tree$children[[1]]$Get("ac", filterFun = function(x) isLeaf(x) & x$is_head),
                ft = my_tree$children[[1]]$Get("ft", filterFun = function(x) isLeaf(x) & x$is_head),
                it = "",
                is_copy = F, filterFun = isRoot)
    my_tree$children[[1]]$Set(ac = 0, ft = 0, filterFun = function(x) isLeaf(x) & x$is_head)
  } else if (ifelse(length(my_tree$lb == my_tree$children[[2]]$lb)== 0 || is.na(my_tree$children[[2]]$lb) || is.na(my_tree$lb),
                    F,my_tree$lb == my_tree$children[[2]]$lb)){
    # set attributes for the resulting labelled phrase
    my_tree$Set(mc = NA,
                ml = NA,
                mr = NA,
                ac = my_tree$children[[2]]$Get("ac", filterFun = isNotLeaf)[1],
                ft = my_tree$children[[2]]$Get("ft", filterFun = isNotLeaf)[1],
                it = "",
                is_copy = F, filterFun = isRoot)
    my_tree$children[[2]]$ac <- 0
    my_tree$children[[2]]$ft <- 0
  }
  return(my_tree)
}
}
# AGREE FUNCTION, agreement is carried out under sisterhood
agreeMC <- function(my_tree){
  # get agreement conditions
  agree_feats_l <- my_tree$Get("ac", filterFun = function(x) isLeaf(x) & x$position == 1)
  feats_r <- my_tree$Get("ft", filterFun = isNotLeaf)
  agree_left <- which(agree_feats_l == feats_r)
  if(length(agree_left)>0){agree_feats_l[agree_left] <- 0}
  my_tree$Set(ac = agree_feats_l, filterFun = function(x) isLeaf(x) & x$position == 1)
  
  agree_feats_r <- my_tree$Get("ac", filterFun = function(x) x$position == 2)
  feats_l <- my_tree$Get("ft", filterFun = function(x) isLeaf(x) & x$position == 1)
  agree_right <- which(agree_feats_r == feats_l)
  if(length(agree_right)>0){agree_feats_r[agree_right] <- 0}
  my_tree$Set(ac = agree_feats_r, filterFun = function(x) x$position == 2)
  
  return(my_tree)
}

