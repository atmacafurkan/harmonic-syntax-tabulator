library(tidyverse)
library(magrittr)

# Recursive functions to write trees
## draw simple linear tree with only labels
linear_tree <- function(my_tree){
  if (length(my_tree$children)>1){
  written <- paste0(ifelse(my_tree$name == 0,"",my_tree$name),
                    "[",my_tree$children[[1]]$name,ifelse(is.na(my_tree$children[[1]]$ft),"", paste0("(",my_tree$children[[1]]$ft,")"))," ",linear_tree(my_tree$children[[2]]),"]")
  }else{
  written <- paste0(my_tree$name,ifelse(is.na(my_tree$ft),"", paste0("(",my_tree$ft,")")))
  }
  return(written)
}

## draw latex trees for representation
latex_tree <- function(my_tree){
  if (length(my_tree$children)>1){
    written <- paste0("[",
                      ifelse(my_tree$name==0,"",paste0(my_tree$name)),
                      "[", 
                      ifelse(my_tree$children[[1]]$is_copy,"DPc",my_tree$children[[1]]$name),
                      ifelse(my_tree$children[[1]]$ft==0,"", paste0("$_{",my_tree$children[[1]]$ft,"}$")),
                      ifelse(my_tree$children[[1]]$ac==0,"", paste0("$_{A:",my_tree$children[[1]]$ac,"}$")),
                      "]"," ",latex_tree(my_tree$children[[2]]),"]")
  }else{
    written <- paste0("[",
                      my_tree$name,
                      ifelse(my_tree$ft==0,"", paste0("$_{",my_tree$ft,"}$")),
                      ifelse(my_tree$ac==0,"", paste0("$_{A:",my_tree$ac,"}$")),
                      "]")
  }
  return(written)
}

## draw latex_tree trees linearly
latex_linear_tree <- function(my_tree){
  if (length(my_tree$children)>1){
    written <- paste0(ifelse(my_tree$name==0,"",paste0("$_{",my_tree$name,"}$")),
                      
                      "[", my_tree$children[[1]]$name,ifelse(is.na(my_tree$children[[1]]$ft),"",
                                                       paste0("$_{",my_tree$children[[1]]$ft,"}$"))," ",latex_linear_tree(my_tree$children[[2]]),"]")
  }else{
    written <- paste0(ifelse(my_tree$is_copy,paste0(my_tree$name,"$_{copy}$"),my_tree$name),
                      ifelse(is.na(my_tree$ft),"", paste0("$_{",my_tree$ft,"}$")),
                      ifelse(is.na(my_tree$ac),"", paste0("$_{A:",my_tree$ac,"}$")))
  }
  return(written)
}

