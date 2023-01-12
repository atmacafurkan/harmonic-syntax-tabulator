# EXTERNAL MERGE, merge items from the numeration, requires a numeration df as an argument
mergeMC <- function(right_arg, left_arg = NA, numeration){
  if (is.na(left_arg)){
    # if there is no left argument
    field_node <- which(numeration$it == right_arg)
    new_node <- as.Node(numeration[which(numeration$it == right_arg),] %>% mutate(pathString = right_arg))
  }else{
    # start a new node
    new_node <- Node$new("0")
    # set attributes of the node if the input is a character
    if (is.character(left_arg)){
      new_left <- as.Node(numeration[which(numeration$it == left_arg),] %>% mutate(pathString = "left_arg"))
      new_node$AddChildNode(new_left)
    }else{
      # set left node name
      left_arg$Set(name = "left_arg", filterFun = isRoot)
      new_node$AddChildNode(left_arg)}
    # set attributes of the node if the input is a character
    if (is.character(right_arg)){
      new_right <- as.Node(numeration[which(numeration$it == right_arg),] %>% mutate(pathString = "right_arg"))
      new_node$AddChildNode(new_right)
    }else{
      # set right node name
      right_arg$Set(name = "right_arg", filterFun = isRoot)
      new_node$AddChildNode(right_arg)} 
    
    # set fields of the resulting merge
    new_node$Set(mc = "", ac = "", ft = "", lb = 0, it = 0, is_copy = F, filterFun = isRoot)
  }
  
  # assign unique ids
  # add range ids for unique identification
  new_node$Set(range_id = 1:length(new_node$Get("lb")))
  
  # renew domination counts and merge violations
  new_node$Set(n_dominator = 0)
  new_node %<>% recurseMC()
  return(new_node)
}

# INTERNAL MERGE, iteratively merge existing items, requires an empty my_list as a global list vector
moveMC <- function (recurse_tree, input_tree = recurse_tree){
  if (recurse_tree$leafCount > 1){
    # MOVE LEFT CHILD
    # create empty node
    new_left <- Node$new("0", mc = "", ac = "", ft = "", lb = 0, it = "", n_dominator = 0, is_copy = F)
    # add moved item
    my_left <- Clone(recurse_tree$left_arg)
    new_left$AddChildNode(my_left)
    # clone input for second child
    input_left <- Clone(input_tree)
    # range id_s to be reset 
    reset_left <- my_left$Get("range_id")[-1]
    # check if you are moving a phrase (more than one node)
    if (is_empty(reset_left)){ # if it is not a phrase just change is_copy
      # set moved elements' is_copy, using it to change is_copy can create a problem but it is working for now 
      input_left$Set(is_copy = T, filterFun = function(x){x$range_id %in% my_left$Get("range_id")})
    } else { # if it is a phrase, trim anything but the head node from the moved position
      input_left$Set(keep_me = T)
      input_left$Set(keep_me = F, filterFun = function(x){x$range_id %in% reset_left})
      # set moved elements' is_copy 
      input_left$Set(is_copy = T, filterFun = function(x){x$range_id %in% my_left$Get("range_id")})
      # trim anything but the phrase
      Prune(input_left, function(x) x$keep_me)
    }
    # add the second child
    new_left$AddChildNode(input_left)
    # rename children for left_arg and right_arg
    new_left$Set(name= "left_arg", filterFun = function(x){x$position == 1} & isNotRoot(x))
    new_left$Set(name= "right_arg", filterFun = function(x){x$position == 2})
    # reset unique id
    alt_left <- Clone(new_left)
    alt_left$Set(range_id = 1:length(new_left$Get("lb")))
    # renew domination counts and merge violations
    alt_left$Set(n_dominator = 0)
    alt_left %<>% recurseMC()
    # add the resulting tree to the list
    my_list <<- append(my_list,alt_left)
    
    # MOVE RIGHT CHILD
    # create empty node
    new_right <- Node$new("0", mc = "", ac = "", ft = "", lb = 0, it = "", n_dominator = 0, is_copy = F)
    # add moved item
    my_right <- Clone(recurse_tree$right_arg)
    new_right$AddChildNode(my_right)
    # clone input for second child
    input_right <- Clone(input_tree)
    # range id_s to be reset
    reset_right <- my_right$Get("range_id")[-1]
    # check if you are moving a phrase (more than one node)
    if (is_empty(reset_right)){ # if it is not a phrase just change is_copy
      # set moved elements is_copy 
      input_right$Set(is_copy = T, filterFun = function(x){x$range_id %in% my_right$Get("range_id")})
    } else { # if it is a phrase, trim anything but the head node from the moved position
      input_right$Set(keep_me = T)
      input_right$Set(keep_me = F, filterFun = function(x){x$range_id %in% reset_right})
      # set moved elements is_copy
      input_right$Set(is_copy = T, filterFun = function(x){x$range_id %in% my_right$Get("range_id")})
      # trim anything but the phrase
      Prune(input_right, pruneFun = function(x) x$keep_me)
    }
    # add the second child
    new_right$AddChildNode(input_right)
    # rename children for left_arg and right_arg
    new_right$Set(name= "left_arg", filterFun = function(x){x$position == 1} & isNotRoot(x))
    new_right$Set(name= "right_arg", filterFun = function(x){x$position == 2})
    # reset unique id
    alt_right <- Clone(new_right)
    alt_right$Set(range_id = 1:length(new_right$Get("lb"))) 
    # renew domination counts and merge violations
    alt_right$Set(n_dominator = 0)
    alt_right %<>% recurseMC()
    # add the resulting tree to the list
    my_list <<- append(my_list, alt_right)
  }
  # recursively call the function on the left child
  if (my_left$leafCount > 1){
    moveMC(my_left, input_tree)
  }
  # recursively call the function on the right child
  if (my_right$leafCount > 1){
    moveMC(my_right, input_tree)
  }
}

# LABELLING OPERATION, labels the root of a tree carries up dominating daughters ac and ft values
labelMC <- function(input_tree){
  my_tree <- Clone(input_tree)
  if (my_tree$lb!=0){
    return(my_tree)
  } else {
    #master_lb <- c("D"=1,"V"=2,"v"=3,"T"=4,"C"=5)
    master_lb <- c("D"=1,"V"=2,"v1"=3,"v2"=4,"T"=5,"C"=6)
    left_lb <- my_tree$left_arg$Get("lb")[1]
    right_lb <- my_tree$right_arg$Get("lb")[1]
    winner_lb <- ifelse(left_lb>right_lb,left_lb,right_lb) %>% as.integer()
    my_tree$Set(lb = winner_lb,
                it = str_replace_all(paste0(names(master_lb[winner_lb]),"P"),"NAP","0"),
                filterFun = isRoot)
    
    # carry up mc, ac, and ft features for agreeMC
    if (left_lb>right_lb){
      # ac and ft feats
      my_tree$Set(ft=my_tree$left_arg$Get("ft")[1], ac=my_tree$left_arg$Get("ac")[1], filterFun = isRoot)
      my_tree$left_arg$Set(ac = "", ft = "", filterFun = function(x){x$range_id == my_tree$left_arg$Get("range_id")[1]})
      # mc feats
      mc_left <- my_tree$left_arg$Get("mc")[1] %>% str_split(",") %>% unlist()
      lb_right <- my_tree$right_arg$Get("lb")[1]
      if (any(mc_left == lb_right, na.rm = T)){
        my_tree$Set(mc = mc_left[-which(mc_left == lb_right)], filterFun = isRoot)
      } else {
        my_tree$Set(mc = mc_left, filterFun = isRoot)
        }
    } else if (right_lb>left_lb){
      # ac and ft feats
      my_tree$Set(ft=my_tree$right_arg$Get("ft")[1], ac=my_tree$right_arg$Get("ac")[1], filterFun = isRoot)
      my_tree$right_arg$Set(ac = "", ft = "", filterFun = function(x){x$range_id == my_tree$right_arg$Get("range_id")[1]})
      # mc feats
      mc_right <- my_tree$right_arg$Get("mc")[1] %>% str_split(",") %>% unlist()
      lb_left <- my_tree$left_arg$Get("lb")[1]
      if (any(mc_right == lb_left, na.rm = T)){
        my_tree$Set(mc = mc_right[-which(mc_right == lb_left)], filterFun = isRoot)
      } else {
        my_tree$Set(mc = mc_right, filterFun = isRoot)
        }
    }
    # renew domination counts
    my_tree$Set(n_dominator = 0)
    my_tree %<>% recurseMC()
    return(my_tree)
  }
}

# TRICKLE DOWN, iteratively establishes domination counts embedded into labelMC, mergeMC, and moveMC
recurseMC <- function(active_tree, output_tree = active_tree){ 
  # get n_dominators
  n_doms <- output_tree$Get("n_dominator")
  # create a new
  my_tree <- Clone(active_tree)
  # learn mother_lab
  mother_lab <- my_tree$Get("lb")[1] 
  
  # update left
  left_lab <- Get(my_tree$children[1],"lb")[1]
  if (is.null(left_lab)){
    left_lab <- mother_lab
  }
  
  if (mother_lab > left_lab){
    # increase global domination
    range_ids <- my_tree$left_arg$Get("range_id")
    n_doms[range_ids] <- my_tree$left_arg$Get("n_dominator")+1
    # increase local domination
    my_tree$left_arg$Set(n_dominator = my_tree$left_arg$Get("n_dominator")+1)
    output_tree$Set(n_dominator = n_doms)
  }
  
  # update right
  right_lab <- Get(my_tree$children[2],"lb")[1]
  if (is.null(right_lab)){
    right_lab <- mother_lab
  }
  
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
    recurseMC(my_tree$left_arg,output_tree)
  }
  
  # recursively call the function on the right child
  if (is.null(my_tree$right_arg)){} else if (my_tree$right_arg$leafCount > 1){ # check if there is right_arg, if more than 1
    recurseMC(my_tree$right_arg,output_tree)
  }
  return(output_tree)
}

# AGREE FUNCTION, agreement is carried out under sisterhood at the mother node. 
# this can work recursively too, but I dont think it is of any benefit
agreeMC <- function(input_tree){
  my_tree <- Clone(input_tree)
  if(isLeaf(my_tree)){# if it is a leaf, return tree
    return(my_tree)} else {
      node_l <- my_tree$left_arg$Get("range_id")[1]
      node_r <- my_tree$right_arg$Get("range_id")[1]
      # agree left
      left_ac <- my_tree$left_arg$Get("ac")[1] %>% str_split("-") %>% unlist()
      lefter_ft <- my_tree$right_arg$Get("ft")[1] %>% str_split("-") %>% unlist()
      
      if (any(left_ac == lefter_ft)){ # sometimes ac is completely empty, check if there is any match
        match_number <- which(left_ac == lefter_ft)
        left_ac[match_number] <- "0" # turn matches into 0 in ac
        lefter_ft[match_number] <- "0" # turn matches into 0 in feats
        left_ac %<>% paste(collapse = "-") # collapse the resulting string in acs
        lefter_ft %<>% paste(collapse="-") # collapse the resulting string in feats
        my_tree$left_arg$Set(ac = left_ac, filterFun = function(x) x$range_id == node_l) # assign the new ac set
        my_tree$right_arg$Set(ft = lefter_ft, filterFun = function(x) x$range_id == node_r) # assign the new ft set
      }
      
      # agree right
      right_ac <- my_tree$right_arg$Get("ac")[1] %>% str_split("-") %>% unlist()
      righter_ft <- my_tree$left_arg$Get("ft")[1] %>% str_split("-") %>% unlist()
      
      if (any(right_ac == righter_ft)){ # sometimes ac is completely empty, check if there is any match
        match_number <- which(right_ac == righter_ft)
        right_ac[match_number] <- "0"
        righter_ft[match_number] <- "0"
        right_ac %<>% paste(collapse = "-")
        righter_ft %<>% paste(collapse = "-")
        my_tree$right_arg$Set(ac = right_ac, filterFun = function(x) x$range_id == node_r)
        my_tree$left_arg$Set(ft = righter_ft, filterFun = function(x) x$range_id == node_l)
      }
      # recurse on the child nodes
      #agreeMC(my_tree$left_arg)
      #agreeMC(my_tree$right_arg)
      return(my_tree)
    }
}

# EMPTY AGREEMENT FUNCTION, agreement is carried out without the presence of features
mt_agreeMC <- function(input_tree){
  my_tree <- Clone(input_tree)
  head_ac <- my_tree$Get("ac")[1] %>% str_split("-") %>% unlist() # get head ac features
  if (any(head_ac == 1)){ # if any ac is 1
    my_tree$Set(mt_ac = paste(head_ac, collapse = "-"), filterFun = isRoot) # set empty agreements on
    head_ac[which(head_ac == 1)] <- "0" # set all acs to 0
    head_ac %<>% paste(collapse = "-") # collapse new acs
    my_tree$Set(ac = head_ac, filterFun = isRoot) # set the new
  }
  if(isNotLeaf(my_tree)){
      # agree left
      left_ac <- my_tree$left_arg$Get("ac")[1] %>% str_split("-") %>% unlist()
      node_l <- my_tree$left_arg$Get("range_id")[1]
      
      if (any(left_ac == 1)){ # sometimes ac is completely empty, check if there is any 1
        my_tree$left_arg$Set(mt_ac = paste(left_ac, collapse = "-"), filterFun = function(x) x$range_id == node_l) # set mt_ac values
        left_ac[which(left_ac == 1)] <- "0" # turn 1s into 0
        left_ac %<>% paste(collapse = "-") # collapse the resulting string
        my_tree$left_arg$Set(ac = left_ac, filterFun = function(x) x$range_id == node_l) # assign the new ft set
      }
      # agree right, mirror of left
      right_ac <- my_tree$right_arg$Get("ac")[1] %>% str_split("-") %>% unlist()
      node_r <- my_tree$right_arg$Get("range_id")[1]
      
      if (any(right_ac == 1)){ 
        my_tree$right_arg$Set(mt_ac = paste(right_ac, collapse = "-"), filterFun = function(x) x$range_id == node_r)
        right_ac[which(right_ac == 1)] <- "0"
        right_ac %<>% paste(collapse = "-")
        my_tree$right_arg$Set(ac = right_ac, filterFun = function(x) x$range_id == node_r)
      }
    }
  return(my_tree)
}






