library(tidyverse)
library(magrittr)


df_der <- readRDS("basic_derivation.rds")
solution <- readRDS("basic_solution.rds")
names(solution) <- NULL


df_der %>% mutate(harmony = rowSums(sweep(.[,5:17], MARGIN = 2, solution, '*'))) %>% group_by(cycle_number) %>%
  mutate(is_min = ifelse(harmony == min(harmony), T, F), is_winner = ifelse(freq ==1, T,F)) %>% ungroup() %>%
  subset(is_min != is_winner) %>% nrow()
