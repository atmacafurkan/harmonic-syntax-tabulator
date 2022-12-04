library(tidyverse)
library(magrittr)

# Recursive functions to write trees
## draw simple linear tree with only labels recursively
linear_tree <- function(my_tree){
  if(isLeaf(my_tree)){
    # Leaf node
    written <- paste0("[",
                      ifelse(my_tree$it == 0, "", 
                             ifelse(my_tree$is_copy == 1, paste0(my_tree$it,"$_c$"), my_tree$it)),
                      "$_{",
                      ifelse(my_tree$is_copy == 1, "", my_tree$ft),
                      "}$", " ",
                      "$_{a:",
                      ifelse(my_tree$is_copy == 1, "", my_tree$ac),
                      "}$",
                      "]")
  } else {
    # Non-leaf node
    left_str <- linear_tree(my_tree$left_arg)
    right_str <- linear_tree(my_tree$right_arg)
    written <- paste0("[",
                      ifelse(my_tree$it == 0, "", 
                             ifelse(my_tree$is_copy == 1, paste0(my_tree$it,"$_c$"), my_tree$it)),
                      "$_{",
                      case_when(my_tree$is_copy == 1 ~ "",
                                is.null(my_tree$mc) ~ "",
                                is.na(my_tree$mc) ~ "",
                                !is.null(my_tree$mc) ~ my_tree$mc,
                                T ~ ""),
                      "}$",left_str," ",right_str,"]")
  }
  return(written)
}


