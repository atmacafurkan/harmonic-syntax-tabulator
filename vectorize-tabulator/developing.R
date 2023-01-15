library(tidyverse)
library(magrittr)
library(rlang)
library(philentropy)  # KL function
library(optimx)       # optimizing function
library(data.tree)    # data trees

# use old functions until renewed
source("./machinery2.0/updated_draw_latex.R")
source("./harmonic_syntax.R")

numerations <- list.files(path="./numerations", pattern = "*.csv", full.names = T)

my_num <- import_numeration(numerations[1])

trial <- Merge(my_num[[1]], my_num[[2]]) %>% Label() %T>% print("lb","it","mc","ft","ac","exnum") %>%
  Merge(my_num[[3]]) %>% Label() %T>% print("lb","it","mc","ft","ac","exnum") %>%
  Merge(my_num[[6]]) %>% Label() %T>% print("lb","it","mc","ft","ac","exnum") #%>%
  #Merge(my_num[[4]]) %>% Label() %T>% print("lb","it","mc","ft","ac","exnum") %>%
  #Merge(my_num[[5]]) %>% Label() %T>% print("lb","it","mc","ft","ac","exnum")

# merge all the possible phrases from the numeration for the first step
comnbine_2 <- lapply(seq_along(my_num[-1]), function(i){
  Merge(my_num[[i]], my_num[[1]])})

# derivation function
derivate <- function(input_tree = "first", input_numeration){
  if(input_tree == "first"){ # if it is the first step of the derivation
    output_trees <- lapply(input_numeration[-1], function(i) Merge(i, input_numeration[[1]]) )
  } else { # if it is not the first step of the derivation
    ## For merge
    # clone tree to remove associations
    my_tree <- Clone(input_tree)
    
    # add tree leaves to the numeration
    my_numeration <- append(input_numeration, my_tree$leaves)
  }
  return(output_trees)
}












