library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)
library(optimx)       # minimizing optimizer function
library(philentropy)  # KL calculating function
library(xtable)

source("harmonic_syntax.R")

eval_basic <- list.files(path = "./basic_numeration/", pattern = "*.rds$", full.names = T)[4] %>% readRDS() %>%
  mutate(across(where(is.list), ~ map_chr(.x, as.character)))
eval_ewe <- list.files(path = "./ewe_numeration/", pattern = "*.rds$", full.names = T)[4] %>% readRDS() %>%
  mutate(across(where(is.list), ~ map_chr(.x, as.character)))

df_eval <- rbind(eval_basic, eval_ewe)

# my_optimization <- weight_optimize(df_eval, c(4:15))
# saveRDS(my_optimization,"ewe_agree_basic_optimization.rds")

my_optimization <- readRDS("ewe_agree_basic_optimization.rds")

optimized_weights <- my_optimization$par
names(optimized_weights) <- colnames(df_eval)[4:15]
optimized_weights
optimized_weights %<>% round()
optimized_weights
# optimized_weights[4:12] <- optimized_weights[4:12] + 2 # add markedness bias to markedness constraints
# optimized_weights

trial <- df_eval[,4:15] %>% as.data.frame() %>% mutate_all(as.integer) %>% data.matrix()
df_eval %<>% mutate(harmonies = as.numeric(trial %*% optimized_weights))
rm(trial)

df_eval %<>% group_by(input) %>% mutate(is_min = ifelse(min(harmonies) == harmonies, T, F),
                                        is_winner = ifelse(winner == 1, T, F),
                                        check = ifelse(is_min == is_winner, T, F))
View(df_eval)

wanted_cand <- df_eval %>% ungroup() %>% subset(input == "C_a:foc[C T[T DP2[DP2 v[DP1_f:foc,wh v[v V[V DP1c]]]]]]") %>%
  dplyr::select(output, operation, 4:15, harmonies)
View(wanted_cand)

xtable(wanted_cand)