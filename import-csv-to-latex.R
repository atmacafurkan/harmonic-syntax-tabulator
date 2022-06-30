library(tidyverse)
library(magrittr)

# read the derivation
df <- read_tsv("my_derivation4.tsv") 

# enter the names 
colnames(df)[1:3] <- c("input","output","winner")

# save constraint names
constraints <- colnames(df)[4:9]

# read learned data
df_maxent <- read_tsv("my_derivation4_output", col_names = F)
df_maxent <- df_maxent[as.integer(which(str_detect(df_maxent$X2, "Candidate"))):nrow(df_maxent),] %>%
  tail(-1) %>% dplyr::select(1:4)
colnames(df_maxent) <- c("input","output","observed", "predicted")
df_maxent$predicted %<>% as.numeric()
df_maxent %<>% mutate(predicted = round(predicted, digits = 3))

# fill the missing input
df %<>% mutate(optimal = ifelse(winner==1,"\\HandRight","")) %>% dplyr::select(-winner) %>% unite("eval",3:8, sep="&")
df$predicted <- df_maxent$predicted
df %<>% mutate(tree_input = sprintf("\\begin{forest} sn edges %s \\end{forest}", input),
               tree_output = sprintf("\\begin{forest} sn edges %s \\end{forest}", output)) %>%
  unite("cand", predicted, optimal, output, eval, sep="& ")

tableus <- df %>% group_by(input) %>% 
  summarise(eval = paste0(cand, collapse = "\\\\ \\hline"),
  latex_tableau = sprintf("\\begin{tabular}{|%s} \\hline MaxEnt & Cycle:%s & %s & %s \\\\ \\hline\\hline 
                          %s \\\\ \\hline\\end{tabular}",
                          rep("c|", length(constraints)+3) %>% paste0(collapse = ""),
                          cycle_number[1],
                          tree_input[1],
                          paste0(constraints, collapse = "&"),
                          paste0(cand, collapse = "\\\\ \\hline")
                          ),
  cycle = as.integer(cycle_number[1])) %>% dplyr::arrange(cycle)

