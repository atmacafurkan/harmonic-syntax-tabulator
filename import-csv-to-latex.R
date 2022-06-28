library(tidyverse)
library(magrittr)

# read the derivation
df <- read_tsv("my_derivation.tsv") 

# enter the names 
colnames(df)[1:3] <- c("input","output","winner")

# save constraint names
constraints <- colnames(df)[4:8]

# fill the missing input
df %<>% mutate(optimal = ifelse(winner==1,"\\HandRight","")) %>% dplyr::select(-winner) %>% unite("eval",3:7, sep="&")
df %<>% mutate(tree_input = sprintf("\\begin{forest} sn edges %s \\end{forest}", input),
               tree_output = sprintf("\\begin{forest} sn edges %s \\end{forest}", output)) %>%
  unite("cand", optimal,output, eval, sep="&")

tableus <- df %>% group_by(input) %>% 
  summarise(eval = paste0(cand, collapse = "\\\\ \\hline"),
  latex_tableau = sprintf("\\begin{tabular}{|%s} \\hline %s & %s & %s \\\\ \\hline 
                          %s \\\\ \\hline\\end{tabular}",
                          rep("c|", length(constraints)+2) %>% paste0(collapse = ""),
                          cycle_number[1],
                          tree_input[1],
                          paste0(constraints, collapse = "&"),
                          paste0(cand, collapse = "\\\\ \\hline")
                          ),
  cycle = as.integer(cycle_number[1])) %>% dplyr::arrange(cycle)

df_maxent <- read_tsv("my_derivation_output", col_names = F)
