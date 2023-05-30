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
           pathString = "new_arg") # add pathstring value for as.Node function
  
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
  if (my_tree$isLeaf){
    if (my_tree$lb %in% stash){
      # do nothing if the label is in stash
    } else {
      # update stash
      stash %<>% append(my_tree$lb)
      # return item if not in stash
      return(my_nodes)  
    }
    
  } else {
    if (my_tree$left_arg$lb %in% stash){
      # do nothing if the label is in stash
    } else {
      # update stash
      stash %<>% append(my_tree$left_arg$lb)
      # add new node
      my_nodes %<>% append(Clone(my_tree$left_arg)$Set(name = "left_arg", exnum = 1, filterFun = isRoot))  
    }
    
    if (my_tree$right_arg$lb %in% stash){
      # do nothing if the label is in stash
    } else {
      # update stash
      stash %<>% append(my_tree$right_arg$lb)
      # add new node
      my_nodes %<>% append(Clone(my_tree$right_arg)$Set(name = "left_arg", exnum = 1, filterFun = isRoot))  
    }
    return(append(my_nodes, get_subtrees(my_tree$left_arg, stash)) %>% append(get_subtrees(my_tree$right_arg, stash)))
  }
}

# a function to prune copies from their moved position, depricated
prune_copies <- function(input_tree){
  my_tree <- Clone(input_tree)
  if(my_tree$left_arg$exnum == 1){ # if there is exnum violation
    reset_left <- my_tree$left_arg$Get("range_id")[-1] # get range ids to reset
    my_tree$right_arg$Set(keep_me = T) # mark everything to stay
    # get relevant it names
    my_its <- paste0(my_tree$right_arg$Get("it", filterFun = function(x){x$range_id %in% my_tree$left_arg$Get("range_id")}),"c")
    my_tree$right_arg$Set(keep_me = F, filterFun = function(x){x$range_id %in% reset_left}) # mark moved for removal
    my_tree$right_arg$Set(is_copy = T, it = my_its, filterFun = function(x){x$range_id %in% my_tree$left_arg$Get("range_id")}) # mark moved as copy
    Prune(my_tree$right_arg, function(x) x$keep_me) # prune the right argument of moved items
    return(my_tree)
  } else {
    return(my_tree)
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
  
  # modify the new list by merging the left arg from the left_arg list and the right_arg tree
  x <- lapply(seq_along(left_arg), function(i) new_nodes[[i]]$AddChildNode(left_arg[[i]])$AddSiblingNode(right_arg[[i]]))
  rm(x)
  
  # update output numerations, domination counts, eval, and range_ids, prune copies and previous numerations
  new_nodes <- lapply(seq_along(new_nodes), function(i) {
    new_nodes[[i]]$output_num <- my_tree$output_num
    new_nodes[[i]]$Set(output_num = 0, filterFun = function(x) !isRoot(x))
    #new_nodes[[i]] <- prune_copies(new_nodes[[i]])
    new_nodes[[i]]$Set(range_id = 1: length(new_nodes[[i]]$Get("lb")), n_dominator = 0, mt_ac = "") # reset range_id, dominator count, and mt_ac
    new_nodes[[i]]$gen <- "iMerge"
    new_nodes[[i]]
  })
  
  # update output numeration trees for external merge and set operation name
  x <- lapply(seq_along(my_tree$output_num), function(i) {
    new_nodes[[i]]$output_num <- my_tree$output_num[-i]
    new_nodes[[i]]$gen <- "xMerge"
    new_nodes[[i]]
  })
  
  # recalculate domination counts and evaluation after
  new_nodes %<>% lapply(function(i) count_domination(i) %>% form_evaluation())
  
  # reflexive merge
  self_merge <- Clone(input_tree)$Set(mt_ac = "") %>% form_evaluation()
  self_merge$eval$exnum <- 1
  self_merge$eval$operation <- "rMerge"
  self_merge$gen <- "rMerge"
  self_merge$input <- draw_tree(input_tree)
  new_nodes %<>% append(self_merge)
  
  # return the list of trees as a result of all possible Merge operations
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
      # remove the moved values
      right_win$right_arg$ft <- ""
      right_win$right_arg$ac <- ""
      right_win$left_arg$exnum <- 0
      
      # renew domination counts and add eval
      right_win$Set(n_dominator = 0, mt_ac = "")
      right_win$gen <- "Label" # add operation name
      right_win %<>% count_domination() %>% form_evaluation()
      right_win$eval$exnum <- 0 # set exnum constraint to zero for labeling operation
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
      # remove the moved values
      left_win$left_arg$ft <- ""
      left_win$left_arg$ac <- ""
      left_win$left_arg$lb <- 0
      
      # renew domination counts and add eval
      left_win$Set(n_dominator = 0, mt_ac = "")
      left_win$gen <- "Label" # add operation name
      left_win %<>% count_domination() %>% form_evaluation()
      left_win$eval$exnum <- 0 # set exnum constraint to zero for labeling operation
      left_win$input <- draw_tree(input_tree) # add input tree
      return(list(left_win, right_win))
    }
}

# agree function, compare ft and ac features of the last siblings
Agree <- function(input_tree){
  # clone tree to remove connections
  my_tree <- Clone(input_tree)
  
  # if input is not a leaf
  if(!my_tree$isLeaf){
  # agree left
  left_ac <- unlist(str_split(my_tree$left_arg$ac, "-"))
  lefter_ft <- unlist(str_split(my_tree$right_arg$ft, "-"))
  
  if (any(which(left_ac == 1) == which(lefter_ft == 1))){ # check for matching ft and ac values
    match_number <- which(left_ac == lefter_ft)
    left_ac[match_number] <- "0" # turn matches into 0 in ac
    lefter_ft[match_number] <- "0" # turn matches into 0 in feats
    my_tree$left_arg$ac <- paste(left_ac, collapse = "-")
    my_tree$right_arg$ft <- paste(lefter_ft, collapse="-")
  } else if (any(left_ac == 1)){ # empty agreement
    my_tree$left_arg$mt_ac <- paste(left_ac, collapse = "-")
    my_tree$left_arg$ac <- paste(rep("0", length(left_ac)), collapse = "-")
  }
  
  # agree right
  right_ac <- unlist(str_split(my_tree$right_arg$ac, "-"))
  righter_ft <- unlist(str_split(my_tree$left_arg$ft, "-"))
  
  if (any(which(right_ac == 1) == which(righter_ft == 1))){ # check for matching ft and ac values
    match_number <- which(right_ac == righter_ft)
    right_ac[match_number] <- "0"
    righter_ft[match_number] <- "0"
    my_tree$right_arg$ac <- paste(right_ac,collapse = "-")
    my_tree$left_arg$ft <- paste(righter_ft, collapse = "-")
  } else if (any(right_ac == 1)){ # empty agreement
    my_tree$right_arg$mt_ac <- paste(right_ac, collapse = "-")
    my_tree$right_arg$ac <- paste(rep("0", length(right_ac)), collapse = "-")
  }
  }
  
  # empty agree head
  head_ac <- unlist(str_split(my_tree$ac, "-"))
  if (any(head_ac == 1)){
    my_tree$mt_ac <- paste(head_ac, collapse = "-")
    my_tree$ac <- paste(rep("0", length(head_ac)), collapse = "-")
  }
  
  my_tree$gen <- "Agree" # add operation name
  my_tree %<>% form_evaluation() # add eval after agree
  my_tree$eval$lab <- 0 # set labeling constraint to zero for agreement operation
  my_tree$eval$exnum <- 0 # set exnum constraint to zero for agreement operation
  my_tree$input <- draw_tree(input_tree) # add input tree flat
  # add exnum info
  if (my_tree$isLeaf){
    # nothing
  } else {
    my_tree$left_arg$exnum <- 0
  }
  
  
  # check if agreement happened
  if(any(my_tree$Get("ac") == input_tree$Get("ac"))){
    # if no, return empty list
    return(list())
  } else {
    # if yes, return tree
    return(list(my_tree))
  }
}

# EVAL FUNCTIONS #####
# merge constraint, iteratively check downwards
cons_merge <- function(my_tree){
  violations <- 0
  # check if tree has children
  if (isNotLeaf(my_tree)){
    # check left
    left_mc <- as.integer(unlist(str_split(my_tree$left_arg$mc,",")))
    for_left_lb <- my_tree$right_arg$lb 
    if (any(for_left_lb == left_mc, na.rm = T)){
      NULL 
    } else if (for_left_lb == 0){
      violations %<>% +1
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
  
  }
  # return result for leaf node
  violation <- tibble(merge_cond = violations)
  return(violation)
}

# labelling constraint, counts unlabeled phrases
cons_derive <- function(my_tree){
  violation <- tibble(exnum = ifelse(my_tree$left_arg$exnum == 1, 1, 0),
                      lab = ifelse(my_tree$lb == 0, 1, 0))
  return(violation)
}

# markedness constraints, counts agree conditions, empty agreements, and features under domination only works for leaves
cons_marked <- function(my_tree){
  violation <- tibble(mt_acs = my_tree$Get("mt_ac"), acs = my_tree$Get("ac"), feats = my_tree$Get("ft"), # get the marked feats and agreements
                      copy = my_tree$Get("is_copy"), n_doms = my_tree$Get("n_dominator")) %>% # get copy and domination counts
    mutate(acs = ifelse(copy == 1, "", acs),
           feats = ifelse(copy == 1, "", feats)) %>% # remove feats if it is a copied element
    tidyr::separate(col = mt_acs, into = c("case_mt","foc_mt","wh_mt"), sep = "-", fill = "right") %>% # separate mt_acs into columns
    tidyr::separate(col = acs, into = c("case_a","foc_a","wh_a"), sep = "-", fill = "right") %>% # separate acs into columns
    tidyr::separate(col = feats, into = c("case","foc","wh"), sep = "-", fill = "right") %>% # separate feats into columns
    mutate_all(as.integer) %>% replace(is.na(.),0) %>% # turn into numeric values and fill missing
    mutate_at(vars(4:9), list(~ . * n_doms)) %>% # multiply each row by domination count
    summarise(case_agr = sum(case_a), foc_agr = sum(foc_a), wh_agr = sum(wh_a), # summarize agreement conditions
              case_mt = sum(case_mt), foc_mt = sum(foc_mt), wh_mt = sum(wh_mt), # summarize empty agreement
              case = sum(case), foc = sum(foc), wh = sum(wh)) # summarize features and copy 
  return(violation)
}

# a function to form evaluation for a tree
form_evaluation <- function(input_tree){
  my_tree <- Clone(input_tree)
  if (my_tree$isLeaf){
    my_eval <- tibble(output = draw_tree(my_tree), operation = "x", exnum = 0, lab = 0, merge_cond = 0,
                      case_agr = 0, foc_agr = 0, wh_agr = 0,
                      case_mt = 0, foc_mt = 0, wh_mt = 0,
                      case = 0, foc = 0, wh = 0)
    my_tree$eval <- my_eval
  return(my_tree)
  } else {
  my_eval <- do.call(cbind, list(tibble(output = draw_tree(my_tree), operation = my_tree$gen), cons_derive(my_tree), cons_merge(my_tree), cons_marked(my_tree)))
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
  tree
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

