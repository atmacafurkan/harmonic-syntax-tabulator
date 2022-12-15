library(tidyverse)
library(magrittr)

df <- read_tsv("./derivations/my_derivation6_output2")
colnames(df) <- paste0("x",1:length(df))

cons_weights <- tibble(constraints = c("lab","mc","foc_agr","case_agr","wh","copy","mt","exnum"),
                       weights = df$x2[(which(str_detect(df$x1, "after"))+1):(which(str_detect(df$x1, "after"))+8)] %>% as.numeric()) %>%
  mutate(weights = round(weights))
# EUREKA ONLY CYCLE 17 CHANGES WITH THE ADDITION OF 2 MARKEDNESS BIAS
# exnum
cons_weights$weights[8] <- 40
# mt
cons_weights$weights[7] <- 3
# copy
cons_weights$weights[6] <- 1 + 2
# wh
cons_weights$weights[5] <- 7 + 2
# case_agr
cons_weights$weights[4] <- 26 + 2
#foc_agr
cons_weights$weights[3] <- 6 + 2

evals <- read_tsv("./derivations/my_derivation6.tsv")
evals %<>% add_column(mt = rep(0,nrow(.)), .after = 9)
evals$mt[92] <- 1
for (each in 1:nrow(evals)){
  evals[each, 4:11] <- evals[each, 4:11]*cons_weights$weights
}

evals %<>% rowwise() %>% mutate(H = sum(c(lab+mc+foc_agr+case_agr+wh+copy+mt+exnum)))


evals %<>% group_by(cycle_number) %>% mutate(minH = min(H))

evals %<>% subset(freq == 1) %>% mutate(is_correct = (minH==H))
View(evals)
