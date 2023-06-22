library(tidyverse)
library(magrittr)
library(rlang)
library(data.tree)
library(optimx)       # minimizing optimizer function
library(philentropy)  # KL calculating function
library(xtable)

source("harmonic_syntax.R")

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

rotate_text <- function(x) {
  paste0("\\rotatebox{90}{", gsub("_", "\\\\_", x), "}")
}

tabulate_latex <- function(my_file, my_table, my_weights){
  if(!file.exists(my_file)){
    file.create(my_file)
  }
  my_caption <- paste("Input", my_table$input[1] %>% as.character() %>% str_replace_all("_", "\\\\_")) 
  
  my_table %<>% dplyr::select(winner, output, 4:7, 10, harmonies)
  my_weights2 <- c(my_weights[1:4],my_weights[7])
  my_table %<>% dplyr::select(winner, output, 4:22, harmonies, harmonies2)
  colnames(my_table)[3:7] <- paste0(colnames(my_table)[3:7],"$^{", my_weights2, "}$")
  my_lines <- capture.output(print(xtable(my_table, caption = my_caption), include.rownames = F, caption.placement = "top", 
                                   sanitize.colnames.function = rotate_text,
                                   size = "\\footnotesize"))
  cat(my_lines, "\n", file = my_file, append = T)
}

export_derivation <- function(my_eval, my_optimization, new_file){
  df_eval <- readRDS(my_eval) %>% mutate(across(where(is.list), ~ map_chr(.x, as.character)))
  con_weights <- readRDS(my_optimization) %>% round() %>% as.numeric() %>% data.matrix()
  my_calc <- df_eval[,4:22] %>% as.data.frame() %>% mutate_all(as.integer) %>% data.matrix()
  df_eval$harmonies <- as.numeric(my_calc %*% con_weights)
  bias <- 3
  con_weights2 <- c(con_weights[1:9], con_weights[10:13]+bias, con_weights[14:16], con_weights[17:19]+bias)
  df_eval$harmonies2 <- as.numeric(my_calc %*% con_weights2)
  df_eval %<>% group_by(input) %>% mutate(min_1 = ifelse(min(harmonies) == harmonies,T,F),
                                          min_2 = ifelse(min(harmonies2) == harmonies2,T,F),
                                          is_changed = (min_1 != min_2)) %>% ungroup()
  #df_eval %<>% group_by(input) %>% mutate(is_changed = ifelse(min(harmonies) != min(harmonies2),T,F)) %>% ungroup()
  df_eval %<>% mutate_all(as.character)
  df_eval$input <- factor(df_eval$input, levels = unique(df_eval$input))
  df_eval %<>% split(.$input)
  x <- lapply(df_eval, tabulate_latex, my_weights = con_weights, my_file = sprintf("./%s/latex_tables.txt", new_file))
  }

a <- c("./unaccusative_numeration/my_eval.rds")
b <- c("./combined_numeration/my_optimization.rds")
c <- c("combined_numeration")

export_derivation(a,b,c)
