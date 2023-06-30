library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)
library(optimx)       # minimizing optimizer function
library(philentropy)  # KL calculating function
library(xtable)

source("harmonic_syntax.R")
#### IMPORT AND OPTIMIZE ####
eval_basic <- list.files(path = "./derivations/basic_numeration/", pattern = "*.rds$", full.names = T)[4] %>% readRDS() %>% 
  mutate(across(where(is.list), ~ map_chr(.x, as.character))) %>% mutate(der = "der_ext")
eval_unaccusative <- list.files(path = "./derivations/unaccusative_numeration/", pattern = "*.rds$", full.names = T)[4] %>% readRDS() %>% 
  mutate(across(where(is.list), ~ map_chr(.x, as.character))) %>% mutate(der = "der_unacc")  
phrasal <- list.files(path = "./derivations/phrasal_movement/", pattern = "*.rds$", full.names = T)[4] %>% readRDS() %>% 
  mutate(across(where(is.list), ~ map_chr(.x, as.character))) %>% mutate(der = "der_phrasal") 

df_eval <- rbind(eval_basic, eval_unaccusative) %>%
  rbind(phrasal) %>% mutate(input = paste(input, der)) %>% dplyr::select(-der)

if (!dir.exists("./derivations/combined_phrasal")){dir.create("./derivations/combined_phrasal")}

saveRDS(df_eval, "./derivations/combined_phrasal/my_eval.rds")

if (file.exists("./derivations/combined_phrasal/my_optimization.rds")){
  combined_weights <- readRDS("./derivations/combined_phrasal/my_optimization.rds")
  } else {
    combined_weights <- weight_optimize(df_eval, 4:22)
    combined_weights <- tibble(weights = combined_weights$par) %>% t()
    colnames(combined_weights) <- colnames(df_eval)[4:22]
    saveRDS(combined_weights, "./derivations/combined_phrasal/my_optimization.rds")
  }

df_eval %<>% mutate(across(where(is.list), ~ map_chr(.x, as.character)))
con_weights <- combined_weights %>% round() %>% as.numeric() %>% data.matrix()

my_calc <- df_eval[,4:22] %>% as.data.frame() %>% mutate_all(as.integer) %>% data.matrix()
df_eval$harmonies <- as.numeric(my_calc %*% con_weights)
df_eval %<>% mutate_at(1, as.integer)
df_eval %<>% mutate_at(4:22, as.integer)
df_eval %<>% mutate_at(24:25, as.integer)
df_eval %<>% mutate_at(c("input","output"), as.character)

df_eval %<>% tidyr::separate(input, into = c("input","derivation"), sep = "der_")
colnames(df_eval)[4:22] <- paste0(colnames(df_eval)[4:22], "$^{", con_weights, "}$")
df_eval$derivation <- factor(df_eval$derivation, levels = unique(df_eval$derivation))
df_eval$input <- factor(df_eval$input, levels = unique(df_eval$input))
df_eval %<>% dplyr::select(winner, input, operation, output, 4:22, derivation, H= harmonies)
df_eval %<>% split(.$derivation)

prune_columns <- function(my_df){ # a function to remove constraints never violated in a derivation step
  dplyr::select_if(my_df, function(x) any(unique(x) != 0))
}

df_ext <- df_eval[[1]] %>% dplyr::select(-derivation) %>% 
  mutate(input = factor(input, levels = unique(input))) %>%
  split(.$input) %>% lapply(prune_columns)

df_unacc <- df_eval[[2]] %>% dplyr::select(-derivation) %>% 
  mutate(input = factor(input, levels = unique(input))) %>%
  split(.$input) %>% lapply(prune_columns) 

df_phrasal <- df_eval[[3]] %>% dplyr::select(-derivation) %>% 
  mutate(input = factor(input, levels = unique(input))) %>%
  split(.$input) %>% lapply(prune_columns) 

### EXPORT FOR LATEX ####
rotate_text <- function(x) { 
  paste0("\\rotatebox{90}{", gsub("_", "\\\\_", x), "}") 
}

tabulate_latex <- function(my_file, my_table){ 
  if(!file.exists(my_file)){ 
    file.create(my_file) 
    my_con <- file(my_file)
    writeLines("\n", my_con)
    close(my_con)
  } 
  my_caption <- paste("Input", my_table$input[1] %>% as.character() %>% str_replace_all("_", "\\\\_"))  
  my_table %<>% dplyr::select(-input)
  my_lines <- capture.output(print(xtable(my_table, caption = my_caption), include.rownames = F, caption.placement = "top",  
                                   sanitize.colnames.function = rotate_text, 
                                   size = "\\footnotesize")) 
  my_con <- file(my_file, "a")
  writeLines(my_lines,"\n", con = my_con) 
  close(my_con)
} 


df_phrasal %>% lapply(tabulate_latex, my_file = "./latex_tables/phrasal_movement_tables.txt")



