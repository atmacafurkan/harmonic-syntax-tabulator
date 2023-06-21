library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)
library(optimx)       # minimizing optimizer function
library(philentropy)  # KL calculating function
library(xtable)

source("harmonic_syntax.R")

dt <- import_numeration("./numerations/basic_numeration.csv")

draw_tree2 <- function(my_tree){
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
  print(written)
}

