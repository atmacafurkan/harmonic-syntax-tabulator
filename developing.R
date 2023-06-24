library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)
library(optimx)       # minimizing optimizer function
library(philentropy)  # KL calculating function
library(xtable)

source("harmonic_syntax.R")

#### IMPORT AND OPTIMIZE ####
eval_basic <- list.files(path = "./basic_numeration/", pattern = "*.rds$", full.names = T)[4] %>% readRDS() %>% 
  mutate(across(where(is.list), ~ map_chr(.x, as.character))) %>% mutate(der = "der_ext")
eval_unaccusative <- list.files(path = "./unaccusative_numeration/", pattern = "*.rds$", full.names = T)[4] %>% readRDS() %>% 
  mutate(across(where(is.list), ~ map_chr(.x, as.character))) %>% mutate(der = "der_unacc")  
ewe_default <- list.files(path = "./ewe_numeration/", pattern = "*.rds$", full.names = T)[4] %>% readRDS() %>% 
  mutate(across(where(is.list), ~ map_chr(.x, as.character))) %>% mutate(der = "der_ewe_def") 
ewe_matrix <- list.files(path = "./ewe_matrix/", pattern = "*.rds$", full.names = T)[4] %>% readRDS() %>% 
  mutate(across(where(is.list), ~ map_chr(.x, as.character))) %>% mutate(der = "der_ewe_mtr") 

df_eval <- rbind(eval_basic, eval_unaccusative) %>%
  rbind(ewe_default) %>% rbind(ewe_matrix) %>% mutate(input = paste(input, der)) %>% dplyr::select(-der)

if (!dir.exists("combined_numeration")){dir.create("combined_numeration")}

saveRDS(df_eval, "./combined_numeration/my_eval.rds")

if (file.exists("./combined_numeration/my_optimization.rds")){
  combined_weights <- readRDS("./combined_numeration/my_optimization.rds")
  } else {
    combined_weights <- weight_optimize(df_eval, 4:22)
    combined_weights <- tibble(weights = combined_weights$par) %>% t()
    colnames(combined_weights) <- colnames(df_eval)[4:22]
    saveRDS(combined_weights, "./combined_numeration/my_optimization.rds")
  }

df_eval %<>% mutate(across(where(is.list), ~ map_chr(.x, as.character)))
con_weights <- combined_weights %>% round() %>% as.numeric() %>% data.matrix()

my_calc <- df_eval[,4:22] %>% as.data.frame() %>% mutate_all(as.integer) %>% data.matrix()
df_eval$harmonies <- as.numeric(my_calc %*% con_weights)
bias <- 4
con_weights2 <- c(con_weights[1:12], con_weights[13:19]+bias)
df_eval$harmonies2 <- as.numeric(my_calc %*% con_weights2)
df_eval %<>% group_by(input) %>% mutate(min_1 = ifelse(min(harmonies) == harmonies,T,F),
                                        min_2 = ifelse(min(harmonies2) == harmonies2,T,F),
                                        is_changed = (min_1 != min_2)) %>% ungroup()

df_eval %<>% mutate_at(1, as.integer)
df_eval %<>% mutate_at(4:22, as.integer)
df_eval %<>% mutate_at(24:26, as.integer)
df_eval %<>% mutate_at(c("input","output"), as.character)
df_eval %<>% dplyr::select(-min_1,-min_2, -is_changed)
df_eval %<>% tidyr::separate(input, into = c("input","derivation"), sep = "der_")
colnames(df_eval)[4:22] <- paste0(colnames(df_eval)[4:22], "$^{", con_weights, "}$")

df_eval$derivation <- factor(df_eval$derivation, levels = unique(df_eval$derivation))
df_eval$input <- factor(df_eval$input, levels = unique(df_eval$input))
df_eval %<>% dplyr::select(winner, input, operation, output, 4:22, derivation, H= harmonies, H2 = harmonies2)
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

df_ewe_def <- df_eval[[3]] %>% dplyr::select(-derivation) %>% 
  mutate(input = factor(input, levels = unique(input))) %>%
  split(.$input) %>% lapply(prune_columns) 

df_ewe_matrix <- df_eval[[4]] %>% dplyr::select(-derivation) %>% 
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

df_unacc %>% lapply(tabulate_latex, my_file = "./latex_tables/basic_unacc_tables")

df_ext %>% lapply(tabulate_latex, my_file = "./latex_tables/basic_external_tables")

df_ewe_def %>% lapply(tabulate_latex, my_file = "./latex_tables/ewe_default_tables")

df_ewe_matrix %>% lapply(tabulate_latex, my_file = "./latex_tables/ewe_matrix_tables")

