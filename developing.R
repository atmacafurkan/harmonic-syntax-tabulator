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

my_optimization <- readRDS("./optimization_results/ewe_agree_basic_optimization.rds")

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
                                        check = ifelse(is_min == is_winner, T, F),
                                        harmonies = as.character(harmonies),
                                        winner = ifelse(winner == 1, "\\HandRight", ""),) %>% ungroup()

df_eval$input <- factor(df_eval$input, levels = unique(df_eval$input))

dl <- df_eval %>% split(df_eval$input, drop = F)

my_table <- dl[[3]]

# Define a function to rotate text
rotate_text <- function(x) {
  paste("\\rotatebox{90}{", gsub("_", "-", x), "}", sep = "")
}

small_text <- function(x) {
  paste0("\\small ", gsub("_", "-", x))
}

tabulate_latex <- function(my_file, my_table){
  if(!file.exists(my_file)){
    file.create(my_file)
  }
  my_caption <- paste("Input", my_table$input[1] %>% as.character() %>% str_replace_all("_", "-"))
  my_table %<>% dplyr::select(operation, winner, output, 4:15, harmonies)
  my_lines <- print(xtable(my_table, caption = my_caption), include.rownames = F, caption.placement = "top", 
                    sanitize.colnames.function = rotate_text,
                    size = "\\small")
  cat(my_lines, "\n", file = my_file, append = T)
}

lapply(dl, tabulate_latex, my_file = "./latex_tabulars/ewe_basic_agree_latex.txt")
