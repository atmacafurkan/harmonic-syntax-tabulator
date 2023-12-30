library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)
library(optimx)       # minimizing optimizer function
library(philentropy)  # KL calculating function
library(xtable)

source("harmonic_syntax.R")

dt <- readRDS("./derivations/phrasal_movement/my_derivation.rds")


# DISPLAY FUNCTIONS ####
# a function to draw a simple tree for display as output, recursive
draw_forest <- function(my_tree){
  feat_names <- c("case","foc","wh")
  if(isLeaf(my_tree)){
    # Leaf node
    written <- paste0("[", 
                      my_tree$it,
                      "$_{",
                      ifelse(any(unlist(str_split(my_tree$ft, "-"))==1),
                             paste0("F:", paste(feat_names[which(unlist(str_split(my_tree$ft, "-"))==1)], collapse = ",")),
                             ""),
                      ifelse(any(unlist(str_split(my_tree$ac, "-"))==1),
                             paste0("A:", paste(feat_names[which(unlist(str_split(my_tree$ac, "-"))==1)], collapse = ",")),
                             ""),
                      "}$",
                      "]")
  } else {
    # Non-leaf node
    left_str <- draw_forest(my_tree$left_arg)
    right_str <- draw_forest(my_tree$right_arg)
    written <- paste0("[",
                      ifelse(my_tree$it ==0, "", my_tree$it),
                      "$_{",
                      ifelse(any(unlist(str_split(my_tree$ft, "-"))==1),
                             paste0("F:", paste(feat_names[which(unlist(str_split(my_tree$ft, "-"))==1)], collapse = ",")),
                             ""),
                      ifelse(any(unlist(str_split(my_tree$ac, "-"))==1),
                             paste0("A:", paste(feat_names[which(unlist(str_split(my_tree$ac, "-"))==1)], collapse = ",")),
                             ""),
                      "}$",
                      left_str, " ", right_str,"]")
  }
  return(written)
}

my_lines <- lapply(dt, draw_forest) %>% unlist() %>% paste0("\\begin{forest}",.,"\\end{forest}\\\\")
my_file <- "forest_trees.txt"

if(!file.exists(my_file)){ 
  file.create(my_file) 
  my_con <- file(my_file)
  writeLines("\n", my_con)
  close(my_con)
} 

my_con <- file(my_file, "a")
writeLines(my_lines,"\n", con = my_con) 
close(my_con)
