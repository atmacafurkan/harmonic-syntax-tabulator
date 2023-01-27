library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)    # data trees
library(igraph)
library(ggraph)

# use old functions until renewed
source("./harmonic_syntax.R")

numerations <- list.files(path="./numerations", pattern = "*.csv", full.names = T)
my_num <- import_numeration(numerations[1])

saveRDS(my_num[[1]], "my_tree.rds")

new_nodes <- fn_cycle(my_num[[1]])

new_nodes %>% sapply(function(i) i$eval) %>% t() %>% as.data.frame() %>% rownames_to_column(var = "candidate")

fn_plotter <- function(my_tree){
  my_tree$Set(name = my_tree$Get("it"))
  tree.igraph <- data.tree::as.igraph.Node(my_tree)
  V(tree.igraph)$class <- names(V(tree.igraph))
  my_plot <- ggraph(tree.igraph,layout='tree')+
    geom_edge_link(arrow=arrow(length=unit(2,'mm')),end_cap=circle(3,'mm'))+
    geom_node_label(aes(label=class))+
    theme_void()
  my_plot
}


my_tree <- new_nodes[[1]] %>% Label() %>% plot()

fn_plotter(x)
