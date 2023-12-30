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

# combine basic derivations and remove mrc
df_eval <- rbind(eval_basic, eval_unaccusative) %>% mutate(input = paste(input, der)) %>% dplyr::select(-der) %>% dplyr::select(-mrc)

# create folder for the example
if (!dir.exists("./derivations/combined_basic")){dir.create("./derivations/combined_basic")}

# save derivations
saveRDS(df_eval, "./derivations/combined_basic/my_eval.rds")

# check if there is an optimization already, if not run the optimizer and save.
if (file.exists("./derivations/combined_basic/my_optimization.rds")){
  combined_weights <- readRDS("./derivations/combined_basic/my_optimization.rds")
} else {
  combined_weights <- weight_optimize(df_eval, 4:21)
  combined_weights <- tibble(weights = combined_weights$par) %>% t()
  colnames(combined_weights) <- colnames(df_eval)[4:21]
  saveRDS(combined_weights, "./derivations/combined_basic/my_optimization.rds")
}

df_eval %<>% mutate(across(where(is.list), ~ map_chr(.x, as.character)))
con_weights <- combined_weights %>% round() %>% as.numeric() %>% data.matrix()
my_calc <- df_eval[,4:21] %>% as.data.frame() %>% mutate_all(as.integer) %>% data.matrix()

df_eval %<>% mutate_at(1, as.integer)
df_eval %<>% mutate_at(4:21, as.integer)
df_eval %<>% mutate_at(23:25, as.integer)
df_eval %<>% mutate_at(c("input","output"), as.character)
df_eval %<>% dplyr::select(-min_1,-min_2, -is_changed)
df_eval %<>% tidyr::separate(input, into = c("input","derivation"), sep = "der_")
colnames(df_eval)[4:21] <- paste0(colnames(df_eval)[4:21], "$^{", con_weights, "}$")

df_eval$derivation <- factor(df_eval$derivation, levels = unique(df_eval$derivation))
df_eval$input <- factor(df_eval$input, levels = unique(df_eval$input))
df_eval %<>% dplyr::select(winner, input, operation, output, 4:21, derivation, H= harmonies)
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

### EXPORT FOR LATEX ####
rotate_text <- function(x) { 
  gsub("_", "\\\\_", x)
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

df_unacc %>% lapply(tabulate_latex, my_file = "./latex_tables/basic_unacc_tables_no-mrc.txt")

df_ext %>% lapply(tabulate_latex, my_file = "./latex_tables/basic_external_tables_no-mrc.txt")

