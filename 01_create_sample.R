library(tidyverse)
library(purrr)
library(qdap)
library(qdapDictionaries)
library(googlesheets4)
library(tidytext)

source("lib/text_helpers.R")

source("lib/extract_excerpts.R")
source("lib/clean_excerpts.R")
source("lib/filter_excerpts.R")
source("lib/mark_unformatted.R")

lapply(3:5, extract_excerpts) %>%
  bind_rows() -> ocr_excerpts

# go here to get the hand labeled sample to check how well filter_unformatted() works compared to
# human labeling that leaves out unformatted passages
# https://docs.google.com/spreadsheets/d/1qNosh58vPgun2q0pq4_o0ppmO_sZQ1JRiJzOyOCtTXA/edit#gid=357759634

handsel_g5_passages <- read_sheet("https://docs.google.com/spreadsheets/d/1qNosh58vPgun2q0pq4_o0ppmO_sZQ1JRiJzOyOCtTXA/edit#gid=357759634",
                                  sheet = "grade5_hand_sampled_excerpts")

handsel_g5_passages %>%
  filter_unwanted() %>%
  replace_na(list(selected = 0)) %>%
  mutate(selected = as.logical(selected),
         enum = row_number(), .before = selected) %>%
  clean_excerpts() %>%
  mark_unformatted() -> handsel_g5_passages_marked
  
# handsel_g5_passages_marked %>%
#   filter(selected == F, pred_selected == T) %>%
#   View()

(handsel_g5_passages_marked %>%
    count(selected, pred_selected) -> g5_pred_stats)

(g5_pred_stats$n[1] + g5_pred_stats$n[4]) / sum(g5_pred_stats$n) * 100

ocr_excerpts %>%
  filter_len_keep_unq() %>%
  filter_unwanted() %>%
  mutate(enum = row_number(), .before = excerpt) %>%
  clean_excerpts() %>%
  mark_unformatted() -> all_excerpts

# set.seed(30)
# 
# all_excerpts %>%
#   filter(pred_selected == TRUE) %>%
#   sample_n(30) %>%
#   View()
# 
# # 5 issues found
# t.test(c(rep(0, 5), rep(1, 15)))

all_excerpts %>%
  filter(pred_selected == TRUE) -> well_formatted_excerpts

(raw_excerpts <- well_formatted_excerpts)

set.seed(10)
(prompt_excerpts <- sample_n(well_formatted_excerpts, 10))

set.seed(5)
(check_excerpts <- sample_n(well_formatted_excerpts, 5))

set.seed(25)
(sample1_excerpts <- sample_n(well_formatted_excerpts, 25))

set.seed(250)
(sample2_excerpts <- sample_n(well_formatted_excerpts, 250))

save(raw_excerpts, check_excerpts, sample1_excerpts, sample2_excerpts, file = "01_excerpts.Rdata")
write_csv(prompt_excerpts, "exports/prompt_excerpts.csv", na = "")
