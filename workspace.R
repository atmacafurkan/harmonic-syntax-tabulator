library(tiyverse)
library(magrittr)

source("./machinery/eval_functions.R")
source("./machinery/gen_functions.R")
source("./machinery/draw_trees.R")
source("./machinery/cyclic_operator.R")
source("./machinery/depricated_functions.R")
cons_merge <- old_cons_merge


df <- read_tsv("my_derivation4_output")
colnames(df) <- paste0("x",1:length(df))

cons_weights <- tibble(constraints = c("lab","mc","case_agr","wh","copy","exnum"),
                       weights = df$x2[(which(str_detect(df$x1, "after"))+1):(which(str_detect(df$x1, "after"))+6)] %>% as.numeric()) %>%
  mutate(weights = round(weights))

# exnum
cons_weights$weights[6] <- 26
# copy
cons_weights$weights[5] <- 2 + 2
# wh
cons_weights$weights[4] <- 9 + 2
# case_agr
cons_weights$weights[3] <- 17 + 2


evals <- read_tsv("my_derivation4.tsv")

for (each in 1:nrow(evals)){
  evals[each, 4:9] <- evals[each, 4:9]*cons_weights$weights
}

evals %<>% rowwise() %>% mutate(H = sum(c(lab+mc+case_agr+wh+copy+exnum)))

evals %<>% group_by(cycle_number) %>% mutate(minH = min(H))

evals %<>% subset(freq == 1) %>% mutate(is_correct = (minH==H))
