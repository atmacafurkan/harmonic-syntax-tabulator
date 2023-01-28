library(tidyverse)
library(magrittr)

feat_names <- c("case","foc","wh")

# Recursive functions to write trees
## draw simple linear tree with only labels recursively
linear_tree <- function(my_tree){
  if(isLeaf(my_tree)){
    # Leaf node
    written <- paste0("[",
                      case_when(my_tree$it == 0 ~ "",
                                my_tree$is_copy == 1 ~ paste0(my_tree$it,"c"),
                                T ~ my_tree$it),
                      "$_{",
                      case_when(my_tree$is_copy == 1 ~ "",
                                any(unlist(str_split(my_tree$ft, "-"))==1) ~ paste(feat_names[which(unlist(str_split(my_tree$ft, "-"))==1)], collapse = ","),
                                T ~ ""),
                      case_when(my_tree$is_copy == 1 ~ "",
                                any(unlist(str_split(my_tree$ac, "-"))==1) ~ paste0("a:",paste(feat_names[which(unlist(str_split(my_tree$ac, "-"))==1)], collapse = ",")),
                                T ~ ""),
                      "}$",
                      "]")
  } else {
    # Non-leaf node
    left_str <- linear_tree(my_tree$left_arg)
    right_str <- linear_tree(my_tree$right_arg)
    written <- paste0("[",
                      ifelse(my_tree$it == 0, "", 
                             ifelse(my_tree$is_copy == 1, paste0(my_tree$it,"c"), my_tree$it)),
                      "$_{",
                      case_when(my_tree$is_copy == 1 ~ "",
                                any(unlist(str_split(my_tree$ft, "-")) ==1) ~ paste(feat_names[which(unlist(str_split(my_tree$ft, "-"))==1)], collapse = ","),
                                T ~ ""),
                      case_when(my_tree$is_copy == 1 ~ "",
                                any(unlist(str_split(my_tree$ac, "-"))==1) ~ paste0("a:", paste(feat_names[which(unlist(str_split(my_tree$ac, "-"))==1)], collapse = ",")),
                                T ~ ""),
                      "}$ ",left_str," ",right_str,"]")
  }
  return(written)
}


