library(tidyverse)
library(magrittr)

df_der <- readRDS("basic_derivation.rds")
solution <- readRDS("basic_solution.rds")

df <- df_der %>% mutate(harmony = rowSums(sweep(.[,5:17], MARGIN = 2, solution, '*'))) %>%
  mutate(winner = ifelse(freq == 1, T, F)) %>%
  dplyr::select(cycle_number, operation, input, output, winner,
                names(which(solution != 0)), harmony) 
rm(df_der)

df %<>% group_by(cycle_number) %>% mutate(cand_no = 1:n()) %>% mutate(operation = str_replace(operation,"[:punct:]", " "))

winner_no <- "1"

total_printing <- character()

for (each in 1:length(unique(df$cycle_number))){
  c_step <- df %>% subset(cycle_number == each)
  full_table <- c_step %>% ungroup() %>% dplyr::select(winner, operation, lab, mc, case_agr, case_mt, copy, exnum, harmony) %>%
    mutate(cand = sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s",
                          ifelse(winner, "\\HandRight",""),
                          operation,
                          lab, mc, case_agr, case_mt, copy, exnum, round(harmony)))
  candidates <- paste(full_table$cand, collapse = "\\\\\\hline\n")
  # modify output numbers
  c_step$cand_no <- paste0(winner_no,".",c_step$cand_no)
  
  printing <- sprintf(
  "\\hbox{\\small\\begin{tabular}{|rl||c|c|c|c|c|c|c|}\\hline
  \\multicolumn{2}{|l||}{I: \\hbox{\\tiny \\begin{forest} %s \\end{forest}}} & \\textsc{lab} & \\textsc{mc} & \\textsc{agr} & \\textsc{mt} & \\textsc{copy} & \\textsc{Exnum} & H\\\\\\hline
  %s\\\\\\hline
  \\end{tabular}}\n\n \\framebreak \n"
  ,
  c_step$input[1],candidates)
  total_printing %<>% append(printing)
  # set new winner no
  winner_no <<- c_step$cand_no[which(c_step$winner)]
}

file_con <- file("./basic_derivation_tableaux.txt")
writeLines(total_printing, file_con, sep = "\n")
close(file_con)



trial <- matrix(data = c(1,0,1,0,4,5,0,0,
                         1,0,1,0,0,7,0,0,
                         0,0,1,0,4,3,0,0,
                         0,0,0,0,4,3,1,0),
                nrow = 4, ncol = 8, byrow = T)

trial3 <- matrix(data = c(1,1,1,0,4,5,0,0,
                          1,0,1,0,0,7,0,0,
                          0,1,1,0,4,3,0,0,
                          0,1,0,0,4,3,1,0), 
                 nrow= 4, ncol = 8, byrow = T)

weights <- c(-20,-11,-8,-28,-9,-3,-3,-40)

trial3p <- sweep(trial3, MARGIN = 2, weights, "*") %>% rowSums() %>% exp()
trial2 <- sweep(trial, MARGIN = 2, weights, "*") %>% rowSums() %>% exp()
  
trial2[1] / (trial2[1] + trial2[2] + trial2[3] + trial2[4])
trial2[2] / (trial2[1] + trial2[2] + trial2[3] + trial2[4])
trial2[3] / (trial2[1] + trial2[2] + trial2[3] + trial2[4])
trial2[4] / (trial2[1] + trial2[2] + trial2[3] + trial2[4])

trial3p[1] / (trial3p[1] + trial3p[2] + trial3p[3] + trial3p[4])
trial3p[2] / (trial3p[1] + trial3p[2] + trial3p[3] + trial3p[4])
trial3p[3] / (trial3p[1] + trial3p[2] + trial3p[3] + trial3p[4])
trial3p[4] / (trial3p[1] + trial3p[2] + trial3p[3] + trial3p[4])

