library(tidyverse)
library(magrittr)
library(data.tree)


# an initial numeration from which the merge operator draws futures as attributes
df_numeration <- tibble(it=c("DP", "V","v","T","C"), 
                        mc = c("", "DP", "VP","vP","TP"),
                        ac=c("case","","","",""),
                        ft=c("wh","","case","",""),
                        lb=c("D","V","v","T","C"),
                        is_copy = rep(F,5),
                        is_head = ifelse(nchar(it) ==1, T,F))

# a merge function that can deal with external and internal merge
mergeMC <- function(right_arg, left_arg){
  # right_arg <- dt_trial
  # left_arg <- "DP"
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
    # check if it is internal move, turn on is_copy for the moved item and remove other attributes
    if(any(str_detect(right_arg$Get("it"), left_arg))){
      
      right_arg$Set(is_copy = T,
                    it="",mc="",ac="",ft="",lb="", filterFun = function(x) isLeaf(x) & any(x$Get("it") == left_arg))
      new_node$left_arg$is_moved <- T
    }
    new_node$AddChildNode(right_arg)} 
  # set attributes for the resulting merge phrase
  new_node$Set(mc = "", ac = "", ft = "", is_copy = F, filterFun = isNotLeaf)
  
  # rename the nodes with their item names 
  if(is.character(left_arg)){new_node$left_arg$Set(name = new_node$left_arg$it)}
  if(is.character(right_arg)){new_node$right_arg$Set(name = new_node$right_arg$it)}
  new_node
}

# assign dominated elements
labelMC <- function(my_tree){
  label_order <- c("0","","D","V","v","T","C")
  if(any(class(my_tree) == "Node")){
    if(my_tree$root$name == "0"){
      my_labels <- my_tree$Get("lb") %>% as.vector() %>% unlist() %>% .[2:3]
      new_label <-ifelse(max(which(label_order %in% my_labels))>2 ,
                         label_order[max(which(label_order %in% my_labels))], "0")
      my_tree$Set(name = paste0(new_label,"P"),
                  lb = new_label, 
                  it = new_label,
                  filterFun = isRoot)}
    # add domination information to the children once a  label is formed 
    my_tree$Set(dominated_by = paste(as.vector(my_tree$Get("dominated_by", filterFun = isLeaf)), new_label) %>% str_remove("NA "),
                filterFun = isLeaf)
    dominators <- as.vector(my_tree$Get("dominated_by", filterFun = isLeaf))
    dom_count <- integer()
    for (each in 1:length(dominators)){
      # take the number of unique labels into account
      add_count <- dominators[[each]] %>% str_split(" ") %>% unlist() %>% unique() %>% length()
      dom_count %<>% append(add_count)}
    my_tree$Set(dominator_count = dom_count, filterFun = isLeaf) 
    # remove dominator count for copies
    my_tree$Set(dominator_count = 0, filterFun = function(x) isLeaf(x) & x$is_copy)
 }
  my_tree
}

dt_trial <- mergeMC("DP","V") %>% labelMC() %>%
  mergeMC("v") %>% labelMC() %>%
  mergeMC("DP") %>% labelMC() %>%
  mergeMC("T") %>% labelMC() %>%
  mergeMC("DP") %>% labelMC() %>%
  mergeMC("C") %>% labelMC() %>%
  mergeMC("DP") %>% labelMC()

print(dt_trial, "dominator_count","ft")
plot(dt_trial)

