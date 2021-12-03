# there are 4 different conditions

# c1 - improve readability by one grade level
# c2 - improve by two grade levels
# c3 - rephrase in a second style 
# c4 - few shot learning, sentence level rephrasing
# c5 - few shot learning, excerpt level rephrasing
# c6 7 and 8 are added to complement conditions 1 to 3

# condition examples (conditions 6 7 8 examples omitted)

# c1
# 
# INPUT
# 
# I rephrased this for my daughter, in plain language a _____ grader can understand:

# c2
# 
# INPUT
# 
# I rephrased this for my daughter, in plain language a _____ grader can understand:

# c3
# 
# My second grader asked me what this passage means:
# """
# INPUT
# """
# I rephrased it for him, in plain language a second grader can understand:
# """

# c4 and c5 are typical few shot prompts

library(tidyverse)
library(qdap)
library(magrittr)

source("lib/text_helpers.R")

load("01_excerpts.Rdata")

condition_names <- c("Improve One Grade Level",
                     "Improve Two Grade Levels",
                     "Rephrase For Second Grader With Title",
                     "5 Training Examples",
                     "11 Training Examples",
                     "Improve One Grade Level With Title",
                     "Improve Two Grade Levels With Title",
                     "Rephrase for Second Grader Without Any Title")

condname <- factor(condition_names, condition_names)

c12_prompt <- read_lines("prompts/rephrase_dynamic_grade.txt") %>%
  paste0(collapse = "\n")

c3_prompt <- read_lines("prompts/rephrase_with_title_second_grade.txt") %>%
  paste0(collapse = "\n")

c4_prompt <- read_lines("prompts/fewshot_5_examples.txt") %>%
  paste0(collapse = "\n")

c5_prompt <- read_lines("prompts/fewshot_11_examples.txt") %>%
  paste0(collapse = "\n")

c67_prompt <- read_lines("prompts/rephrase_with_title_dynamic_grade.txt") %>%
  paste0(collapse = "\n")

c8_prompt <- read_lines("prompts/rephrase_second_grade.txt") %>%
  paste0(collapse = "\n")

excerpts <- sample2_excerpts

# output - # of words * 1.3333

excerpts %>%
  mutate(n_words = word_count(excerpt),
         max_tokens = round(n_words * 1.333333, 0)) %>%
  select(enum, excerpt, grade, n_words, max_tokens) %>%
  expand(nesting(enum, excerpt, grade, n_words, max_tokens), crossing(condname)) %>%
  mutate(one_glevel = case_when(
    grade == 3 ~ "second",
    grade == 4 ~ "third",
    grade == 5 ~ "fourth"
  ),
  two_glevel = case_when(
    grade == 3 ~ "first",
    grade == 4 ~ "second",
    grade == 5 ~ "third"
  ), condcode = as.integer(condname)) -> excerpts_conditions
  
  
excerpts_conditions %>%
  mutate(prompt = case_when(
    condcode == 1 ~ str_replace(c12_prompt, "__INPUT__", excerpt) %>% str_replace("__GRADE__", one_glevel),
    condcode == 2 ~ str_replace(c12_prompt, "__INPUT__", excerpt) %>% str_replace("__GRADE__", two_glevel),
    condcode == 3 ~ str_replace(c3_prompt, "__INPUT__", excerpt),
    condcode == 4 ~ str_replace(c4_prompt, "__INPUT__", excerpt),
    condcode == 5 ~ str_replace(c5_prompt, "__INPUT__", excerpt),
    condcode == 6 ~ str_replace(c67_prompt, "__INPUT__", excerpt) %>% str_replace_all("__GRADE__", one_glevel),
    condcode == 7 ~ str_replace(c67_prompt, "__INPUT__", excerpt) %>% str_replace_all("__GRADE__", two_glevel),
    condcode == 8 ~ str_replace(c8_prompt, "__INPUT__", excerpt)
    
  ),
  stopseq = case_when(
    condcode %in% c(1, 2, 8) ~ NA_character_,
    condcode %in% c(3, 6, 7) ~ '"""',
    condcode %in% c(4, 5) ~ "###"
  )) -> exp_design

save(exp_design, file = "02_exp_design_v2.Rdata")

# write_csv(exp_design, "exports/exp_design.csv", na = "")
