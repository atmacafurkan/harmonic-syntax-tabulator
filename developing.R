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
saveRDS(df_eval, "./combined_numeration/my_eval.rds")

if (file.exists("./combined_numeration/my_optimization.rds")){
  combined_weights <- readRDS("./combined_numeration/my_optimization.rds")
  } else {
    combined_weights <- weight_optimize(df_eval, 4:15)
    combined_weights <- tibble(weights = combined_weights$par) %>% t()
    colnames(combined_weights) <- colnames(df_eval)[4:15]
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
  my_table %<>% dplyr::select(winner, output, 4:15, harmonies, harmonies2)
  colnames(my_table)[3:14] <- paste0(colnames(my_table)[3:14],"$^{" ,my_weights, "}$")
  my_lines <- capture.output(print(xtable(my_table, caption = my_caption), include.rownames = F, caption.placement = "top", 
                                   sanitize.colnames.function = rotate_text,
                                   size = "\\footnotesize"))
  cat(my_lines, "\n", file = my_file, append = T)
}

export_derivation <- function(my_eval, my_optimization, new_file){
  df_eval <- readRDS(my_eval) %>% mutate(across(where(is.list), ~ map_chr(.x, as.character)))
  con_weights <- readRDS(my_optimization) %>% round() %>% as.numeric()
  my_calc <- df_eval[,4:15] %>% as.data.frame() %>% mutate_all(as.integer) %>% data.matrix()
  df_eval$harmonies <- as.numeric(my_calc %*% con_weights)
  con_weights2 <- c(con_weights[1:3],con_weights[4:6]+3, con_weights[7:9], con_weights[10:12]+3)
  df_eval$harmonies2 <- as.numeric(my_calc %*% con_weights2)
  df_eval %<>% mutate_all(as.character)
  df_eval$input <- factor(df_eval$input, levels = unique(df_eval$input))
  df_eval %<>% split(.$input)
  x <- lapply(df_eval, tabulate_latex, my_weights = con_weights, my_file = sprintf("./%s/latex_tables.txt", new_file))
  }

a <- c("./ewe_numeration/my_eval.rds")
b <- c("./combined_numeration/my_optimization.rds")
c <- c("combined_numeration")

export_derivation(a,b,c)