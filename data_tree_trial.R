library(tidyverse)
library(magrittr)
library(data.tree)


# an initial numeration from which the merge operator draws futures as attributes
df_numeration <- tibble(it=c("DP", "V","v","T","C"), 
                        mc = c("", "DP", "VP","vP","TP"),
                        ac=c("case","","","",""),
                        ft=c("wh","","case","",""),
                        lb=c("D","V","v","T","C"),
                        is_copy = c(F,F,F,F,F))


# a merge function that can deal with external and internal merge
mergeMC <- function(right_arg, left_arg){
  # start a new node
  new_node <- Node$new("0")
  # set attributes of the node if the input is a character
  if (is.character(left_arg)){new_node$AddChild("left_arg")
    field_number <- which(df_numeration$it == left_arg)
    new_node$left_arg$Set(
      it = df_numeration$it[field_number],
      mc = df_numeration$mc[field_number],
      ac = df_numeration$ac[field_number],
      lb = df_numeration$lb[field_number],
      ft = df_numeration$ft[field_number],
      is_copy = df_numeration$is_copy[field_number]
    )
  }else{new_node$AddChildNode(left_arg)}
  # set attributes of the node if the input is a character
  if (is.character(right_arg)){new_node$AddChild("right_arg")
    field_number <- which(df_numeration$it == right_arg)
    new_node$right_arg$Set(
      it = df_numeration$it[field_number],
      mc = df_numeration$mc[field_number],
      ac = df_numeration$ac[field_number],
      lb = df_numeration$lb[field_number],
      ft = df_numeration$ft[field_number],
      is_copy = df_numeration$is_copy[field_number]
    )
  }else{
    # check if it is internal move, turn on is_copy for the moved item
    if(any(str_detect(right_arg$Get("it"), left_arg))){
      right_arg$Do(function(node) node$is_copy <- T, filterFun = function(x) GetAttribute(x, "it") == left_arg)
      new_node$left_arg$is_moved <- T
    }
    new_node$AddChildNode(right_arg)} 
  # set attributes for the resulting merge phrase
  new_node$Set(it = "", mc = "", ac = "", ft = "", is_copy = "", filterFun = isNotLeaf)
  
  # rename the nodes with their item names 
  if(is.character(left_arg)){new_node$left_arg$Set(name = new_node$left_arg$it)}
  if(is.character(right_arg)){new_node$right_arg$Set(name = new_node$right_arg$it)}
  new_node
}

labelMC <- function(my_tree){
  label_order <- c("0","","D","V","v","T","C")
  if(any(class(my_tree) == "Node")){
    if(my_tree$root$name == "0"){
      my_labels <- my_tree$Get("lb") %>% as.vector() %>% unlist() %>% .[2:3]
      new_label <- case_when(
        length(which(label_order %in% my_labels))==1 ~ label_order[which(label_order %in% my_labels)],
        diff(which(label_order %in% my_labels))==1 ~ label_order[max(which(label_order %in% my_labels))],
        T ~ "0")
      my_tree$Set(name = new_label,lb = new_label, it = new_label,filterFun = isRoot)}}
  my_tree
}

dt_trial <- mergeMC("DP","V") %>% labelMC() %>%
  mergeMC("v") %>% labelMC() %>%
  mergeMC("T") %>% labelMC() %>% 
  mergeMC("C") %>% labelMC()
my_tree <- dt_trial
print(dt_trial, "lb")
plot(dt_trial)



