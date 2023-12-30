library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)
library(optimx)       # minimizing optimizer function
library(philentropy)  # KL calculating function
library(xtable)
library(vtree)

# a function to read a data frame into a list of tree nodes
import_numeration <- function(path_to_numeration){
  # read numeration from csv
  df <- read.csv(path_to_numeration) %>%
    mutate(mc = ifelse(is.na(mc), "", mc), # fix na values for mc
           pathString = "new_arg", # add pathstring value for as.Node function
           gen = "-",
           n_dominator = 0) 
  
  # turn data frame into a list of data.trees
  numeration_list <- sapply(split(df, 1:nrow(df)), as.Node, USE.NAMES = F)
  
  # add output numeration to each tree
  lapply(seq_along(numeration_list), function(i) numeration_list[[i]]$output_num <- numeration_list[-i])
  
  # draw the tree for each item
  lapply(seq_along(numeration_list), function(i) numeration_list[[i]]$input <- draw_tree(numeration_list[[i]]))
  
  # return the list of trees
  return(numeration_list)
}

# GEN FUNCTIONS #####
# a function to extract subtrees of a tree, recursive, same label is skipped
get_subtrees <- function(input_tree, stash = integer()){
  my_tree <- Clone(input_tree)
  my_nodes <- list()
  if (my_tree$isLeaf){ # if it is a leaf
    if (my_tree$lb %in% stash){
      # do nothing if the label is in stash
    } else {
      # update stash
      stash %<>% append(my_tree$lb)
      # return item if not in stash
      return(my_nodes)  
    }
    
  } else { # if it is not a leaf
    if (my_tree$lb %in% stash){
      # do nothing if the label is in stash
    } else {
      # update stash
      stash %<>% append(my_tree$lb)
    }
    
    if (my_tree$left_arg$lb %in% stash){ # go left
      # do nothing if the label is in stash
    } else {
      # update stash
      stash %<>% append(my_tree$left_arg$lb)
      # add new node
      my_nodes %<>% append(Clone(my_tree$left_arg)$Set(name = "left_arg", filterFun = isRoot))  
    }
    
    if (my_tree$right_arg$lb %in% stash){ # go right
      # do nothing if the label is in stash
    } else {
      # update stash
      stash %<>% append(my_tree$right_arg$lb)
      # add new node
      my_nodes %<>% append(Clone(my_tree$right_arg)$Set(name = "left_arg", filterFun = isRoot))  
    }
    return(append(my_nodes, get_subtrees(my_tree$left_arg, stash)) %>% append(get_subtrees(my_tree$right_arg, stash))) # recurse left and right
  }
}

# update domination counts, recursive
count_domination <- function(active_tree, output_tree = active_tree){ 
  # get n_dominators
  n_doms <- output_tree$Get("n_dominator")
  # create a new
  my_tree <- Clone(active_tree)
  
  # learn mother and children labels
  mother_lab <- my_tree$lb
  left_lab <- my_tree$left_arg$lb
  right_lab <- my_tree$right_arg$lb
  
  # update left
  if (mother_lab != left_lab & mother_lab != 0){
    # increase global domination
    range_ids <- my_tree$left_arg$Get("range_id")
    n_doms[range_ids] <- my_tree$left_arg$Get("n_dominator")+1
    # increase local domination
    my_tree$left_arg$Set(n_dominator = my_tree$left_arg$Get("n_dominator")+1)
    output_tree$Set(n_dominator = n_doms)
  }
  
  # update right
  if (mother_lab != right_lab & mother_lab != 0){
    # increase global domination
    range_ids <- my_tree$right_arg$Get("range_id")
    n_doms[range_ids] <- my_tree$right_arg$Get("n_dominator") + 1
    # increase local domination
    my_tree$right_arg$Set(n_dominator = my_tree$right_arg$Get("n_dominator") + 1)
    output_tree$Set(n_dominator = n_doms)
  }
  
  # recursively call the function on the left child
  if (is.null(my_tree$left_arg)){} else if (my_tree$left_arg$leafCount > 1){ # check if there is left_arg, if more than 1
    count_domination(my_tree$left_arg, output_tree)
  }
  
  # recursively call the function on the right child
  if (is.null(my_tree$right_arg)){} else if (my_tree$right_arg$leafCount > 1){ # check if there is right_arg, if more than 1
    count_domination(my_tree$right_arg, output_tree)
  }
  return(output_tree)
}

# merge function, only takes a tree with a numeration as argument, and returns a list of trees with all possible merges (internal and external)
Merge <- function(input_tree){
  # clone tree to avoid entanglement
  my_tree <- Clone(input_tree)
  
  # construct a list for the left argument from the numeration
  left_arg <- lapply(seq_along(my_tree$output_num), function(i) Clone(my_tree$output_num[[i]])$Set(name = "left_arg", filterFun = isRoot))
  
  # add leaves to the numeration
  if (!my_tree$isLeaf){ # if the input tree is not a leaf
    left_arg %<>% append(get_subtrees(my_tree)) # add the subtrees to trees from the numeration
  }
  
  # use the input as the right argument and make a list of trees (head initial)
  right_arg <- lapply(seq_along(left_arg), function(i) Clone(my_tree)$Set(name = "right_arg", filterFun = isRoot))
  
  # create new node and set default values
  new_nodes <- lapply(seq_along(left_arg), function(i) Node$new("0", mc = "", ac = "0-0-0", ft = "0-0-0", lb = 0, it = 0, is_copy = 0, input = draw_tree(input_tree)))
  
  # modify the new list by merging the left arg from the left_arg list and the right_arg tree, this is basically core of Merge.
  x <- lapply(seq_along(left_arg), function(i) new_nodes[[i]]$AddChildNode(left_arg[[i]])$AddSiblingNode(right_arg[[i]]))
  rm(x)
  
  # update output numerations, domination counts, eval, and range_ids, prune previous numerations
  new_nodes <- lapply(seq_along(new_nodes), function(i) {
    new_nodes[[i]]$output_num <- my_tree$output_num # set output num of root
    new_nodes[[i]]$Set(output_num = 0, filterFun = function(x) !isRoot(x)) # prune numerations of all nodes but the root
    new_nodes[[i]]$Set(range_id = 1: length(new_nodes[[i]]$Get("lb")), n_dominator = 0, mt_ac = "0-0-0") # reset range_id, dominator count, and mt_ac
    new_nodes[[i]]$gen <- "iMerge" # set operation name as internal merge for all
    new_nodes[[i]]
  })
  
  # update output numeration trees for external merge and set operation name
  x <- lapply(seq_along(my_tree$output_num), function(i) {
    new_nodes[[i]]$output_num <- my_tree$output_num[-i] # remove the externally merge nodes from their own numerations
    new_nodes[[i]]$gen <- "xMerge" # correct operation name for external merge 
    new_nodes[[i]]
  })
  rm(x)
  
  # recalculate domination counts and evaluation
  new_nodes %<>% lapply(function(i) count_domination(i) %>% form_evaluation())
  
  # reflexive merge
  self_merge <- Clone(input_tree)$Set(mt_ac = "0-0-0")
  self_merge$eval$operation <- "rMerge"
  self_merge$gen <- "rMerge"
  self_merge %<>% form_evaluation()
  self_merge$eval$exnum <- 1
  self_merge$input <- draw_tree(input_tree)
  new_nodes %<>% append(self_merge)
  
  # return the list of trees as a result of all possible Merge operations (internal, external, reflexive)
  return(new_nodes)
}

# label function, does not calculate dominance
Label <- function(input_tree){
  
  # if there is already a label or if input is a leaf, return an empty list
  if(input_tree$lb != 0 | input_tree$isLeaf){ 
    return(list())} else {
      # Right wins
      # clone to remove connections
      right_win <- Clone(input_tree)
      
      # carry up features
      # gather the values to be passed up
      new_values <- list(lb = right_win$right_arg$lb,
                         ft = right_win$right_arg$ft,
                         ac = right_win$right_arg$ac,
                         it = right_win$right_arg$it)
      # set the new values in the labelled phrase
      rlang::exec(right_win$Set, !!!new_values, filterFun = isRoot)
      
      # renew domination counts and add eval
      right_win$Set(n_dominator = 0, mt_ac = "0-0-0")
      right_win$gen <- "Label" # add operation name
      right_win %<>% count_domination() %>% form_evaluation()
      right_win$input <- draw_tree(input_tree) # add input tree    
      
      # Left wins  
      # clone to remove connections
      left_win <- Clone(input_tree)  
      
      # carry up features
      # gather the values to be passed up
      new_values <- list(lb = left_win$left_arg$lb,
                         ft = left_win$left_arg$ft,
                         ac = left_win$left_arg$ac,
                         it = left_win$left_arg$it)
      # set the new values in the labelled phrase
      rlang::exec(left_win$Set, !!!new_values, filterFun = isRoot)
      
      # renew domination counts and add eval
      left_win$Set(n_dominator = 0, mt_ac = "0-0-0") # reset domination and empty agree
      left_win$gen <- "Label" # add operation name
      left_win %<>% count_domination() %>% form_evaluation() # form evaluation
      left_win$input <- draw_tree(input_tree) # add input tree
      return(list(left_win, right_win)) # return a list of trees
    }
}

# agree function, compare ft and ac features of the last siblings of an unlabelled root or carry out empty agree for a labelled root (partial agree possible)
Agree <- function(input_tree){
  # clone tree to remove connections
  my_tree <- Clone(input_tree)
  
  # if input is not a leaf
  if(!my_tree$isLeaf){
  # agree left
  left_ac <- unlist(str_split(my_tree$left_arg$ac, "-"))
  lefter_ft <- unlist(str_split(my_tree$right_arg$ft, "-"))
  
  if (any(which(left_ac == 1) == which(lefter_ft == 1)) & my_tree$lb==0){ # check for matching ft and ac values
    match_number <- which(left_ac == lefter_ft & my_tree$lb==0)
    left_ac[match_number] <- "0" # turn matches into 0 in ac
    lefter_ft[match_number] <- "0" # turn matches into 0 in feats
    my_tree$left_arg$ac <- paste(left_ac, collapse = "-")
    my_tree$right_arg$ft <- paste(lefter_ft, collapse="-")
  } 
  
  # agree right
  right_ac <- unlist(str_split(my_tree$right_arg$ac, "-"))
  righter_ft <- unlist(str_split(my_tree$left_arg$ft, "-"))
  
  if (any(which(right_ac == 1) == which(righter_ft == 1)) & my_tree$lb==0){ # check for matching ft and ac values
    match_number <- which(right_ac == righter_ft)
    right_ac[match_number] <- "0"
    righter_ft[match_number] <- "0"
    my_tree$right_arg$ac <- paste(right_ac,collapse = "-")
    my_tree$left_arg$ft <- paste(righter_ft, collapse = "-")
  } 
  }
  
  # empty agree head
  head_ac <- unlist(str_split(my_tree$ac, "-"))
  if (any(head_ac == 1)){ # check if there is any agreement feature at the root
    my_tree$mt_ac <- paste(head_ac, collapse = "-")
    my_tree$ac <- paste(rep("0", length(head_ac)), collapse = "-")
  }
  
  my_tree$gen <- "Agree" # add operation name
  my_tree %<>% form_evaluation() # add eval after agree
  my_tree$input <- draw_tree(input_tree) # add input tree flat
  
  # check if agreement happened
  if(any(my_tree$Get("ac") != input_tree$Get("ac"))){
    # if yes, return tree as a list
    return(list(my_tree))
  } else {
    # if no, return empty list
    return(list())
  }
}

# EVAL FUNCTIONS #####
# merge constraint, do not check downwards, only checks left and right
cons_merge <- function(my_tree){
  # check operation
  if(my_tree$isLeaf){ # if the input is a leaf, it cannot violate mc
    violations <- 0
  } else { # in iMerge and xMerge conditions
    violations <- 0
    # check left
    left_mc <- as.integer(unlist(str_split(my_tree$left_arg$mc,",")))
    for_left_lb <- my_tree$right_arg$lb 
    if (any(for_left_lb == left_mc, na.rm = T)){ # if there is any match between the merge features and the label
      NULL 
    } else if (for_left_lb == 0){ # if the merged item lacks a label
      violations %<>% +1
    } else if (is.na(left_mc) || is_empty(left_mc)){ # if the merging item has no merge features
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
    }
  # return result of evaluation
  violation <- tibble(merge_cond = violations)
  return(violation)
}

# label and exhaust numeration constraints, check after a Merge operation only
cons_derive <- function(my_tree){
  violation <- tibble(exnum = ifelse(str_detect(my_tree$gen, "rMerge|iMerge"), 1, 0), # violation of exhaust numeration if it is internal or reflexive merge
                      lab = ifelse(my_tree$lb == 0 & str_detect(my_tree$gen, "Merge"), 1, 0)) # violation if there is no label after Merge
  # return violation
  return(violation)
}

# labeling constraints
cons_label <- function(my_tree){
  label_cons <- tibble(lb_D = 0, lb_V = 0, lb_v = 0, lb_subj = 0, lb_T = 0, lb_C = 0) # default for label constraints is no violation
  if(my_tree$gen == "Label"){ # if the operation is Label
    label_cons[1,my_tree$lb] <- 1 # value of label determines which label constraint is violated
    return(label_cons)
  } else { # if the operation is not Label, no violation
    return(label_cons)
  }
}

# only keep unique domains from the top
get_attributes <- function(input_tree){ # a different implementation of get_subtrees, works almost the same (I did not test it with complex specifiers)
  dt_df <- tibble(lb = input_tree$Get("lb"),
                  mc = input_tree$Get("mc"),
                  ft = input_tree$Get("ft"),
                  ac = input_tree$Get("ac"),
                  mt_ac = input_tree$Get("mt_ac"),
                  n_dominator = input_tree$Get("n_dominator")) %>% 
    distinct(lb, .keep_all = T) %>% dplyr::select(-lb)
  return(dt_df)
}

# markedness constraints, counts agree conditions, empty agreements, and features under domination
cons_marked <- function(my_tree){
  violation <- get_attributes(my_tree) %>%
    tidyr::separate(col = mt_ac, into = c("case_mt","foc_mt","wh_mt"), sep = "-", fill = "right") %>% # separate mt_acs into columns
    tidyr::separate(col = ac, into = c("case_a","foc_a","wh_a"), sep = "-", fill = "right") %>% # separate acs into columns
    tidyr::separate(col = ft, into = c("case","foc","wh"), sep = "-", fill = "right") %>% # separate feats into columns
    mutate(mc = ifelse(mc > 0, 1, 0)) %>%
    mutate_all(as.integer) %>% replace(is.na(.),0) %>% # turn into numeric values and fill missing
    mutate_at(vars(1:7), list(~ . * n_dominator)) %>% # multiply each row by domination count
    summarise(case_mt = sum(case_mt), foc_mt = sum(foc_mt), wh_mt = sum(wh_mt), # summarize empty agreement
              mrc = sum(mc), case_agr = sum(case_a), foc_agr = sum(foc_a), wh_agr = sum(wh_a), # summarize agreement conditions
              case = sum(case), foc = sum(foc), wh = sum(wh)) # summarize features and copy 
  return(violation)
}

# a function to form evaluation for a tree
form_evaluation <- function(input_tree){
  my_tree <- Clone(input_tree)
  if (my_tree$gen == "x"){ # eval is called on every tree, including the numeration. "x" is for those.
    my_eval <- tibble(output = draw_tree(my_tree), operation = "x", exnum = 0, lab = 0, 
                      lb_D = 0, lb_V = 0, lb_v = 0, lb_subj = 0, lb_T = 0, lb_C = 0,
                      merge_cond = 0,
                      case_mt = 0, foc_mt = 0, wh_mt = 0,
                      mrc = 0,
                      case_agr = 0, foc_agr = 0, wh_agr = 0,
                      case = 0, foc = 0, wh = 0)
    my_tree$eval <- my_eval
  return(my_tree)
  } else {
  my_eval <- do.call(cbind, list(tibble(output = draw_tree(my_tree), operation = my_tree$gen), cons_derive(my_tree), cons_label(my_tree), cons_merge(my_tree), cons_marked(my_tree)))
  my_tree$eval <- my_eval
  return(my_tree)
  }
}

# DERIVATION INTERFACE ####
proceed_cycle <- function(input_tree){
  # clone input tree to remove connections
  my_tree <- Clone(input_tree)
  
  # perform all merge operations given the tree with its numeration
  merge_outputs <- Merge(my_tree)
  
  # perform Label on the input tree
  label_outputs <- Label(my_tree)
  
  # perform agree
  agree_outputs <- Agree(my_tree)
  
  # return all three options
  return(c(agree_outputs, label_outputs, merge_outputs))
}

# DISPLAY FUNCTIONS ####
# a function to draw a simple tree for display as output, recursive
draw_tree <- function(my_tree){
  feat_names <- c("case","foc","wh")
  if(isLeaf(my_tree)){
    # Leaf node
    written <- paste0(my_tree$it,
                      case_when(my_tree$is_copy == 1 ~ "",
                                any(unlist(str_split(my_tree$ft, "-"))==1) ~ paste0("_f:", paste(feat_names[which(unlist(str_split(my_tree$ft, "-"))==1)], collapse = ",")),
                                T ~ ""),
                      case_when(my_tree$is_copy == 1 ~ "",
                                any(unlist(str_split(my_tree$ac, "-"))==1) ~ paste0("_a:", paste(feat_names[which(unlist(str_split(my_tree$ac, "-"))==1)], collapse = ",")),
                                T ~ ""))
  } else {
    # Non-leaf node
    left_str <- draw_tree(my_tree$left_arg)
    right_str <- draw_tree(my_tree$right_arg)
    written <- paste0(ifelse(my_tree$it == 0, "", 
                             ifelse(my_tree$is_copy == 1, paste0(my_tree$it,"c"), my_tree$it)),
                      case_when(my_tree$is_copy == 1 ~ "",
                                any(unlist(str_split(my_tree$ft, "-")) ==1) ~ paste0("_f:", paste(feat_names[which(unlist(str_split(my_tree$ft, "-"))==1)], collapse = ",")),
                                T ~ ""),
                      case_when(my_tree$is_copy == 1 ~ "",
                                any(unlist(str_split(my_tree$ac, "-"))==1) ~ paste0("_a:", paste(feat_names[which(unlist(str_split(my_tree$ac, "-"))==1)], collapse = ",")),
                                T ~ ""),
                      "[", left_str, " ", right_str,"]")
  }
  return(written)
}

# a function to plot the data.trees to a png file in www folder
plot_tree <- function(input_tree){
  my_tree <- Clone(input_tree)
  my_tree$Set(name = my_tree$Get("it"))
  tree <- plot(my_tree)
}

# a function to compose eval from a list of trees
compose_eval <- function(my_list){
  sapply(my_list, function(i) i$eval) %>% t() %>% as.data.frame() %>% rownames_to_column(var = "candidate")
}

rotate_text <- function(x) {
  paste0("\\rotatebox{90}{", gsub("_", "\\\\_", x), "}")
}

tabulate_latex <- function(my_file, my_table, my_weights){
  if(!file.exists(my_file)){
    file.create(my_file)
  }
  my_caption <- paste("Input", my_table$input[1] %>% as.character() %>% str_replace_all("_", "\\\\_")) 
  my_table %<>% dplyr::select(winner, output, 4:15, harmonies, harmonies2)
  colnames(my_table)[3:14] <- paste0(colnames(my_table)[3:14],"$^{" ,my_weights, "}$")
  my_lines <- capture.output(print(xtable(my_table, caption = my_caption), include.rownames = F, caption.placement = "top", 
                                   sanitize.colnames.function = rotate_text,
                                   size = "\\footnotesize"))
  cat(my_lines, "\n", file = my_file, append = T)
}

export_derivation <- function(my_eval, my_optimization, new_file){
  df_eval <- readRDS(my_eval) %>% mutate(across(where(is.list), ~ map_chr(.x, as.character)))
  con_weights <- readRDS(my_optimization) %>% round() %>% as.numeric()
  my_calc <- df_eval[,4:15] %>% as.data.frame() %>% mutate_all(as.integer) %>% data.matrix()
  df_eval$harmonies <- as.numeric(my_calc %*% con_weights)
  df_eval %<>% mutate_all(as.character)
  df_eval$input <- factor(df_eval$input, levels = unique(df_eval$input))
  df_eval %<>% split(.$input)
  x <- lapply(df_eval, tabulate_latex, my_weights = con_weights, my_file = sprintf("./%s/latex_tables.txt", new_file))
}

# WEIGHT OPTIMIZER ####
# make sure constructed matrices are not lists
unlisted_matrix <- function(x){as.data.frame(x) %>% mutate_all(as.integer) %>% data.matrix()}

# objective function to be optimized
objective_KL <- function(x, my_tableaux, constraint_range){
  # extract constraint names
  constraints <- colnames(my_tableaux)[constraint_range]
  
  # turn data frame into a list of matrices where each matrix is a single derivation
  tableaux <- my_tableaux %>% split(.$input) %>% map(~ (.x %>% dplyr::select(-input, -output, -candidate, -operation))) %>% lapply(unlisted_matrix)
  
  # frequencies of the candidates for each derivation
  frequencies <- lapply(tableaux, function(x){x[,"winner"]})
  
  # calculate probabilities across the matrices in the list 
  probabilities <- sapply(tableaux, function(the_element) the_element[,constraints] %*% (x*-1)) %>% # calculate harmony values on negative terms
    sapply(function(the_element) exp(the_element)/sum(exp(the_element))) %>% # calculate the probability of each candidate
    lapply(function(x) {colnames(x) <- "probabilities"; x}) # rename the resulting column as probabilities
  
  # combine the frequencies and probabilities of each matrix in the list then transpose it for KLD. Calculate KLD scores
  sum_KL <- Map(cbind, frequencies, probabilities) %>% lapply(t) %>% lapply(KL) %>% unlist() %>% sum()
  
  # return the overall KLD for the whole set of derivations
  return(sum_KL)
}

weight_optimize <- function(the_tableaux, constraints){ # turn the optimizing into a function to be used 
  # anything other than "input" and "output" vector for the data frame "the_tableaux" should be a numeric value. 
  # the data frame should only include the vectors for input, output, frequency, and constraint violations
  n_constraint <- length(constraints) # how many constraints are there?
  # box optimization
  optimal_weights <- optim(par = rep(0, n_constraint), # starting values for the weights is 0
                           fn = objective_KL, # objective function
                           my_tableaux = the_tableaux, # argument to be passed to the objective function
                           constraint_range = constraints, # the range of constraints in the table
                           lower = rep(0, n_constraint), # the lowest that the constraint weights can get
                           upper = rep(100, n_constraint), # the highest that the constraint weights can get
                           method = "L-BFGS-B") # the method of optimization which allows lower bound
  # return the resulting optimization 
  return(optimal_weights)
}