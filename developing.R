library(tidyverse)
library(magrittr)
library(philentropy)  # KL function
library(optimx)       # optimizing function
library(data.tree)

source("./machinery2.0/updated_gen_functions.R")
source("./machinery2.0/updated_eval_functions.R")
source("./machinery2.0/updated_draw_latex.R")
source("./machinery2.0/weight_optimizer.R")

df <- read.csv("basic_numeration.csv", na.strings = "NA") %>% 
  mutate(mc = ifelse(is.na(mc), "", mc))

my_list <- list()
mergeMC("DP1","V",numeration = df) %>% labelMC() %>% mergeMC("v", df) %>% labelMC() %>% moveMC()

my_tree <- Clone(my_list[[4]]) %>% agreeMC() %>% labelMC()
my_list <- list()

# INTERNAL MERGE, iteratively merge existing items, requires an empty my_list as a global list vector
moveMC <- function (recurse_tree, input_tree = recurse_tree){
  # define notin function for pruning
  `%notin%` <<- Negate(`%in%`)
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
      input_left$Set(is_copy = T, filterFun = function(x){x$it %in% my_left$Get("it")})
    } else { # if it is a phrase, trim anything but the head node from the moved position
      input_left$Set(keep_me = T)
      input_left$Set(keep_me = F, filterFun = function(x) x %in% reset_left)
      # set moved elements' is_copy 
      input_left$Set(is_copy = T, filterFun = function(x){x$it %in% my_left$Get("it")})
      # trim anything but the phrase
      #Prune(input_left, function(x) x$keep_me)
    }
    # add the second child
    new_left$AddChildNode(input_left)
    # rename children for left_arg and right_arg
    new_left$Set(name= "left_arg", filterFun = function(x){x$position == 1} & isNotRoot(x))
    new_left$Set(name= "right_arg", filterFun = function(x){x$position == 2})
    # reset unique id
    # new_left$Set(range_id = 1:length(new_left$Get("lb")))
    # renew domination counts and merge violations
    new_left$Set(n_dominator = 0)
    new_left %<>% recurseMC()
    # add the resulting tree to the list
    my_list <<- append(my_list,new_left)
    
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
      input_right$Set(is_copy = T, filterFun = function(x){x$it %in% my_right$Get("it")})
    } else { # if it is a phrase, trim anything but the head node from the moved position
      input_right$Set(keep_me = T)
      input_right$Set(keep_me = F, filterFun = function(x){x$range_id %in% reset_right})
      # set moved elements is_copy
      input_right$Set(is_copy = T, filterFun = function(x){x$it %in% my_right$Get("it")})
      # trim anything but the phrase
      #Prune(input_right, pruneFun = function(x) x$keep_me)
    }
    # add the second child
    new_right$AddChildNode(input_right)
    # rename children for left_arg and right_arg
    new_right$Set(name= "left_arg", filterFun = function(x){x$position == 1} & isNotRoot(x))
    new_right$Set(name= "right_arg", filterFun = function(x){x$position == 2})
    # reset unique id
    # new_right$Set(range_id = 1:length(new_right$Get("lb")))   
    # renew domination counts and merge violations
    new_right$Set(n_dominator = 0)
    new_right %<>% recurseMC()
    # add the resulting tree to the list
    my_list <<- append(my_list, new_right)
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


print(my_tree, "it","lb","mc","is_copy")
moveMC(my_tree$right_arg, my_tree)
print(my_list[[2]], "it","is_copy", "range_id", "keep_me", "n_dominator")
my_list <- list()
moveMC(my_tree)

print(my_list[[4]], "it","is_copy", "range_id", "keep_me","n_dominator")

