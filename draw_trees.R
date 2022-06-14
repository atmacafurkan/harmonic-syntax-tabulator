library(tidyverse)
library(magrittr)

source("data_tree_trial.R")

dt_trial <- mergeMC("DP","V") %>% labelMC() %>% mergeMC("v") %>% labelMC() %>% mergeMC("DP") %>% labelMC()

print(dt_trial, "n_dominator","lb","ac","ft")
cons_lab(dt_trial)
plot(dt_trial)

# Recursive functions to write trees
## draw simple linear tree with only labels
linear_tree <- function(x){
  if (length(x$children)>1){

  written <- paste0(ifelse(x$name==0,"",x$name),
                    "[",x$children[[1]]$name,ifelse(is.na(x$children[[1]]$ft),"", paste0("(",x$children[[1]]$ft,")"))," ",linear_tree(x$children[[2]]),"]")
  }else{
  written <- paste0(x$name,ifelse(is.na(x$ft),"", paste0("(",x$ft,")")))
  }
  return(written)
}

## draw latex trees for represntation
latex_tree <- function(x){
  if (length(x$children)>1){
    written <- paste0("[",ifelse(x$name==0,"",paste0(x$name)),
                      
                      "[", x$children[[1]]$name,ifelse(is.na(x$children[[1]]$ft),"",
                                                      paste0("$_{",x$children[[1]]$ft,"}$")),"]"," ",latex_tree(x$children[[2]]),"]")
  }else{
    written <- paste0("[",
                      ifelse(x$is_copy,paste0(x$name,"$_{copy}$"),x$name),
                      ifelse(is.na(x$ft),"", paste0("$_{",x$ft,"}$")),
                      ifelse(is.na(x$ac),"", paste0("$_{A:",x$ac,"}$")),
                      "]")
  }
  return(written)
}

## draw latex trees linearly
latex_linear_tree <- function(x){
  if (length(x$children)>1){
    written <- paste0(ifelse(x$name==0,"",paste0("$_{",x$name,"}$")),
                      
                      "[", x$children[[1]]$name,ifelse(is.na(x$children[[1]]$ft),"",
                                                       paste0("$_{",x$children[[1]]$ft,"}$"))," ",latex_linear_tree(x$children[[2]]),"]")
  }else{
    written <- paste0(ifelse(x$is_copy,paste0(x$name,"$_{copy}$"),x$name),
                      ifelse(is.na(x$ft),"", paste0("$_{",x$ft,"}$")),
                      ifelse(is.na(x$ac),"", paste0("$_{A:",x$ac,"}$")))
  }
  return(written)
}

# try out the funcitons
linear_tree(dt_trial)
latex_tree(dt_trial)
latex_linear_tree(dt_trial)

