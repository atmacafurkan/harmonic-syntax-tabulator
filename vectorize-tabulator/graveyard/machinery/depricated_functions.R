library(tidyverse)
library(magrittr)


# old merge, does not carry agree conditions and features
old_mergeMC <- function(right_arg, left_arg = NA, numeration){
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
                      name= "DP_copy",
                      filterFun = function(x) isLeaf(x) & any(x$Get("it") == left_arg))
        new_node$left_arg$is_moved <- T
        new_node$left_arg$mc <- NA
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

# old merge 2 can handle internal and external merge, marks moved items and copies, does not carry agree conditions and features, left and right merge condition distinction 
old2_mergeMC <- function(right_arg, left_arg = NA, numeration){
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

# AGREE FUNCTION, handles single specifiers and heads with single agreement conditions
old_agreeMC <- function(my_tree){
  # get agreement conditions
  goal_feats <- my_tree$Get("ac", filterFun = isLeaf)
  
  # get features for specifiers to agree with
  hp_feats <- c(tail(my_tree$Get("ft", filterFun = isLeaf),-1), "0")
  
  # get features for heads to agree with
  sp_feats <- c("0", head(my_tree$Get("ft", filterFun = isLeaf),-1))
  
  # get is_head info about leaves
  head_spec <- my_tree$Get("is_head", filterFun = isLeaf) %>% replace_na(F)
  
  # use a data frame to calculate agreement
  outlook <- tibble(goal_feats, hp_feats, sp_feats, head_spec)
  outlook[is.na(outlook)] <- "0"
  outlook %<>% mutate(new_goals = case_when(
    # agreement for heads with specifiers
    head_spec ~ ifelse(str_detect(sp_feats,goal_feats),"0", goal_feats),
    # agreement for heads
    T ~ ifelse(str_detect(hp_feats,goal_feats),"0", goal_feats)))
  # set agreement on leaves
  my_tree$Set(ac = outlook$new_goals, filterFun = isLeaf)
  
  return(my_tree)
}

# old merge condition constraint, count the number of merge conditions in the structure. works with uniary merge
old_cons_merge <- function(my_tree){
  if (length(my_tree$Get("it", filterFun = isLeaf)) == 1){
    violations <- tibble(mc = length(which(!is.na(my_tree$Get("mc", filterFun = isLeaf) %>% as.vector()))))  
  }else{
    subcat <- my_tree$Get("mc", filterFun = function(x) x$position == 1 & isNotRoot(x))
    merging <- my_tree$Get("lb", filterFun = function(x) x$position == 2)
    violations <- tibble(mc = length(which(merging != subcat)))}
  return(violations)
}

# you feed the step an initial tree and a numeration to use it with
# it returns a list of all possible Merge operations together with an Agree and Label operations, it returns itself last.
# each cycle is a list that contains all relevant information for all the possible outputs given the input
old_cycle_step <- function(my_tree, cycle_numeration){
  
  outputs <- list()
  
  for (each in 1:nrow(cycle_numeration)){
    new_numeration <- cycle_numeration
    new_tree <- Clone(my_tree)
    
    # number of distinct elements in the tree before merge
    count_prev <- length(new_tree$Get("it", filterFun = function(x) isLeaf(x) & !x$is_copy))
    
    new_tree %<>% mergeMC(new_numeration$it[each], numeration = new_numeration)
    
    used_args <- new_tree$Get("it", filterFun = function(x) isLeaf(x) & !x$is_copy) %>% as.vector()
    if (length(which(new_numeration$it %in% used_args)) != 0){
      new_numeration <- new_numeration[-which(new_numeration$it %in% used_args),]}
    
    # list all the used items with features and filter the DPs with changed values
    used_items <- tibble(it = new_tree$Get("it", filterFun = isLeaf) %>% as.vector(),
                         mc = new_tree$Get("mc", filterFun = isLeaf) %>% as.vector(),
                         ac = new_tree$Get("ac", filterFun = isLeaf) %>% as.vector(),
                         ft = new_tree$Get("ft", filterFun = isLeaf) %>% as.vector(),
                         lb = new_tree$Get("lb", filterFun = isLeaf) %>% as.vector(),
                         is_copy = new_tree$Get("is_copy", filterFun = isLeaf) %>% as.vector(),
                         is_head = new_tree$Get("is_head", filterFun = isLeaf) %>% as.vector()) %>%
      subset(!is_head & !is_copy)
    # add copies back to the numeration with features
    new_numeration %<>% rbind(used_items)
    
    # number of distinct elements in the tree after merge
    count_next <- length(new_tree$Get("it", filterFun = function(x) isLeaf(x) & !x$is_copy))
    
    # form the evaluation
    violations <- cons_profile(new_tree) %>% mutate(exnum = NA)
    
    # cross derivational ExNum constraint
    if(count_prev < count_next){
      violations$exnum[1] <- 0 
    } else {
      violations$exnum[1] <- 1
    }
    
    # generate a list of output 
    outputs[[each]] <- list(linear = linear_tree(new_tree),
                            tree_latex = latex_tree(new_tree),
                            tree_linear_latex = latex_linear_tree(new_tree),
                            tree = new_tree,
                            eval = violations,
                            numeration = new_numeration)
    
  }
  
  # return the tree itself
  new_tree <- Clone(my_tree)
  # form the evaluation
  violations <- cons_profile(new_tree) %>% mutate(exnum = 1)
  outputs[[length(outputs)+1]] <- list(linear = linear_tree(new_tree),
                                       tree_latex = latex_tree(new_tree),
                                       tree_linear_latex = latex_linear_tree(new_tree),
                                       tree = new_tree,
                                       eval = violations,
                                       numeration = cycle_numeration)
  
  # agree and return
  new_tree <- Clone(my_tree) %>% agreeMC()
  new_tree2 <- Clone(my_tree) 
  is_different <- any(new_tree$Get("ac") != new_tree2$Get("ac"), na.rm = T)
  if (new_tree$count != 0 & is_different){
    # form the evaluation
    violations <- cons_profile(new_tree) %>% mutate(exnum = 1)
    outputs[[length(outputs)+1]] <- list(linear = linear_tree(new_tree),
                                         tree_latex = latex_tree(new_tree),
                                         tree_linear_latex = latex_linear_tree(new_tree),
                                         tree = new_tree,
                                         eval = violations,
                                         numeration = cycle_numeration)}
  
  # label and return
  new_tree <- Clone(my_tree) %>% labelMC()
  new_tree2 <- Clone(my_tree) 
  is_different <- any(new_tree$Get("name") != new_tree2$Get("name"), na.rm = T)
  if (new_tree$count != 0 & is_different){
    # form the evaluation
    violations <- cons_profile(new_tree) %>% mutate(exnum = 1)
    outputs[[length(outputs)+1]] <- list(linear = linear_tree(new_tree),
                                         tree_latex = latex_tree(new_tree),
                                         tree_linear_latex = latex_linear_tree(new_tree),
                                         tree = new_tree,
                                         eval = violations,
                                         numeration = cycle_numeration)}
  return(outputs)
}

# forms the eval table, without access to the numeration it works with the old_cons_merge
old_cons_profile <- function(my_tree){
  eval_table <- bind_cols(cons_lab(my_tree), cons_merge(my_tree), 
                          cons_agree(my_tree), cons_marked(my_tree)) %>% 
    mutate(input = my_tree$Get("name", filterFun = isRoot))
  return(eval_table)
}

old_labelMC <- function(my_tree){
  if (my_tree$lb!=0){
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
    # not moving anything up currently
    #my_tree$Set(n_dominator = "", filterFun = function(x) x$is_copy)
    if (ifelse(length(my_tree$children[[1]]$is_head)==0,F,my_tree$children[[1]]$is_head)){
      my_tree$Set(mc = NA,
                  ml = NA,
                  mr = NA,
                  #ac = my_tree$children[[1]]$Get("ac", filterFun = function(x) isLeaf(x) & x$is_head),
                  #ft = my_tree$children[[1]]$Get("ft", filterFun = function(x) isLeaf(x) & x$is_head),
                  it = "",
                  is_copy = F, filterFun = isRoot)
      #my_tree$children[[1]]$Set(ac = 0, ft = 0, filterFun = function(x) isLeaf(x) & x$is_head)
    } else if (ifelse(length(my_tree$lb == my_tree$children[[2]]$lb)== 0 || is.na(my_tree$children[[2]]$lb) || is.na(my_tree$lb),
                      F,my_tree$lb == my_tree$children[[2]]$lb)){
      # set attributes for the resulting labelled phrase
      my_tree$Set(mc = NA,
                  ml = NA,
                  mr = NA,
                  #ac = my_tree$children[[2]]$Get("ac", filterFun = isNotLeaf)[1],
                  #ft = my_tree$children[[2]]$Get("ft", filterFun = isNotLeaf)[1],
                  it = "",
                  is_copy = F, filterFun = isRoot)
      #my_tree$children[[2]]$ac <- 0
      #my_tree$children[[2]]$ft <- 0
    }
    return(my_tree)
  }
}


