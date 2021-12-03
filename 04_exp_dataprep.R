library(tidyverse)
library(koRpus.lang.en)
library(koRpus)
library(qdap)
library(magrittr)
library(lsa)

source("lib/calculate_readability_metrics.R")
source("lib/text_helpers.R")

# analyzing machine rephrasing --------------------------------------------

# load("03_gpt3_data/check_excerpts_output.Rdata")
load("03_gpt3_data/sample2_excerpts_output_combined.Rdata")

# cleaning the output
# replacing new lines with spaces
# trimming the white spaces from start and end
# removing the last incomplete sentence (doesnt end with .!?)
exp_output_combined %>%
  mutate(translation = str_replace_all(translation, "\\n", " "),
         translation = trimws(translation)) %>%
  filter(translation != "") %>%
  mutate(translation = sapply(translation, remove_last_incomplete_sent)) -> exp_output_clean

# calculate input and output sentence length, words, and characters -------

exp_output_clean %>%
  mutate(nchars = character_count(excerpt),
         nchars_trn = character_count(translation),
         nwords = word_count(excerpt),
         nwords_trn = word_count(translation),
         nsents = sapply(excerpt, sent_count),
         nsents_trn = sapply(translation, sent_count)) %>%
  dplyr::select(enum, condcode, tnum, nchars, nchars_trn, nwords, nwords_trn, nsents, nsents_trn) -> basic_metrics

# calculate readability metrics from korpus -------------------------------

bind_cols(
  dplyr::select(exp_output_clean, enum, condcode, tnum),
  calculate_readability_metrics(exp_output_clean$excerpt),
  calculate_readability_metrics(exp_output_clean$translation) %>% rename_all(~ paste0(., "_trn"))
) -> readability_metrics


# calculate bert embeddings and readability -------------------------------

# export data for colab notebook processing
# https://colab.research.google.com/drive/1FjnXCO3j-KITg1DM4o5kcZamXL5XGEC_#scrollTo=307GJPj1ELhT

# exp_output_clean %>%
#   dplyr::select(enum, excerpt) %>%
#   distinct() %>%
#   write_csv("exports/bert_excerpts.csv", na = "")
# 
# exp_output_clean %>%
#   dplyr::select(enum, condcode, tnum, translation) %>%
#   write_csv("exports/bert_translations.csv", na = "")

# import back data exported from colab

excerpts_bert_readability <- read_csv("imports/colab_export_excerpts_bert_readability.csv")
excerpts_bertfeats <- read_csv("imports/colab_export_excerpts_bertfeats.csv")

translations_bert_readability <- read_csv("imports/colab_export_translations_bert_readability.csv")
translations_bertfeats <- read_csv("imports/colab_export_translations_bertfeats.csv")

# bert readability scores for input and output

excerpts_bert_readability %>%
  inner_join(translations_bert_readability) %>%
  dplyr::select(enum, condcode, tnum, bert, bert_trn) -> bert_metrics

# bert feature vectors

# cosine similarity tryout

pt1 <- c(0.5, 3.3, 1.2, -7)
pt2 <- c(11.9, -1, 12.8, 3)
pt3 <- c(12.3, 0, 7, 5)

cosine(pt1, pt2)
cosine(cbind(pt1, pt2, pt3))

tibble(
  id1 = c(1,1,1,1,1,1,1,1),
  id2 = c(2,2,2,2,3,3,3,3),
  vec1 = c(pt1, pt1),
  vec2 = c(pt2, pt3)
) %>%
  group_by(id1, id2) %>%
  summarise(csim = cosine(vec1, vec2))


# text vectors for input
excerpts_bertfeats %>%
  gather(key = "featnum", value = "bertfeat", -1) %>%
  mutate(featnum = str_remove(featnum, "feature_"),
         featnum = as.integer(featnum)) -> excerpts_bertfeats_df

# text vectors for output
translations_bertfeats %>%
  gather(key = "featnum", value = "bertfeat_trn", -(1:3)) %>%
  mutate(featnum = str_remove(featnum, "feature_"),
         featnum = as.integer(featnum)) -> translations_bertfeats_df

# similarity between input and output passages

translations_bertfeats_df %>%
  inner_join(excerpts_bertfeats_df, by = c("enum", "featnum")) %>%
  arrange(enum, condcode, tnum, featnum) -> inp_outp_bertfeats

inp_outp_bertfeats %>%
  group_by(enum, condcode, tnum) %>%
  summarise(csim = as.vector(cosine(bertfeat, bertfeat_trn))) %>%
  ungroup() -> inp_outp_csims

hist(inp_outp_csims$csim)

# data to export ----------------------------------------------------------

# metrics

basic_metrics %>%
  inner_join(readability_metrics, by = c("enum", "condcode", "tnum")) %>%
  inner_join(bert_metrics, by = c("enum", "condcode", "tnum")) -> all_metrics

inp_outp_csims

inp_outp_bertfeats

exp_output_clean

# data for analyzing human rephrasing -------------------------------------

prompt_rdf <- read_csv("imports/gsheet_export_prompt_excerpts_rephrased.csv")

prompt_bert_readability <- read_csv("imports/colab_export_prompt_excerpts_rephrased_readability.csv") %>%
  dplyr::select(enum, tnum, bert, bert_trn)

prompt_bert_readability %>%
  mutate(diff = bert_trn - bert)

prompt_inp_bertfeats <- read_csv("imports/colab_export_prompt_excerpt_bertfeats.csv") %>%
  gather(key = "featnum", value = "bertfeat", -(1:2)) %>%
  mutate(featnum = str_remove(featnum, "feature_"),
         featnum = as.integer(featnum))

prompt_outp_bertfeats <- read_csv("imports/colab_export_prompt_rephrased_bertfeats.csv") %>%
  gather(key = "featnum", value = "bertfeat_trn", -(1:2)) %>%
  mutate(featnum = str_remove(featnum, "feature_"),
         featnum = as.integer(featnum))

prompt_inp_bertfeats %>%
  inner_join(prompt_outp_bertfeats, by = c("enum", "tnum", "featnum")) -> prompt_inp_outp_bertfeats

prompt_inp_outp_bertfeats %>%
  group_by(enum, tnum) %>%
  summarise(csim = as.vector(cosine(bertfeat, bertfeat_trn))) %>%
  ungroup() -> prompt_inp_outp_csim

# all metrics
prompt_rdf %>%
  mutate(nchars = character_count(excerpt),
       nchars_trn = character_count(rephrased),
       nwords = word_count(excerpt),
       nwords_trn = word_count(rephrased),
       nsents = sapply(excerpt, sent_count),
       nsents_trn = sapply(rephrased, sent_count)) %>%
  bind_cols(
    calculate_readability_metrics(prompt_rdf$excerpt),
    calculate_readability_metrics(prompt_rdf$rephrased) %>%
      rename_all(~ paste0(., "_trn"))
  ) %>%
  inner_join(prompt_bert_readability, by = c("enum", "tnum")) %>%
  dplyr::select(-excerpt, -rephrased) -> prompt_all_metrics


# rename conditions and final object names --------------------------------------

# renaming conditions in the machine translation set

# creating new altcondname factor
# condname to altcondname mapping
# altcondcode is integer version of the altcondname factor

unique(exp_output_clean$condname)

alt_condition_names <- c("Improve One Grade Level" = "Rephrase One Grade Level Below",
                     "Improve Two Grade Levels" = "Rephrase Two Grade Levels Below",
                     "Rephrase for Second Grader Without Any Title" = "Rephrase for Second Grader",
                     
                     "Improve One Grade Level With Title" = "Rephrase With Title One Grade Level Below",
                     "Improve Two Grade Levels With Title" = "Rephrase With Title Two Grade Levels Below",
                     "Rephrase For Second Grader With Title" = "Rephrase With Title for Second Grader",
                     
                     "5 Training Examples" = "Few Shot Learning 5 Examples",
                     "11 Training Examples" = "Few Shot Learning 11 Examples")

exp_rdf <- exp_output_clean %>%
  mutate(altcondname = alt_condition_names[as.character(condname)],
         altcondname = factor(altcondname, as.vector(alt_condition_names)),
         altcondcode = as.integer(altcondname)) %>%
  arrange(enum, altcondcode, tnum)

exp_rdf %>%
  dplyr::select(condcode, altcondcode) %>%
  distinct() -> condcode_altcondcode_map

# choosing final object names

# machine translation

exp_rdf
(exp_metrics <- all_metrics %>% inner_join(condcode_altcondcode_map, by = "condcode"))
(exp_csims <- inp_outp_csims %>% inner_join(condcode_altcondcode_map, by = "condcode"))
(exp_bertfeats <- inp_outp_bertfeats %>% inner_join(condcode_altcondcode_map, by = "condcode"))
(altcondnames <- unique(exp_rdf$altcondname))

# human translations

prompt_rdf
(prompt_metrics <- prompt_all_metrics)
(prompt_csims <- prompt_inp_outp_csim)
(prompt_bertfeats <- prompt_inp_outp_bertfeats)

# alternative names will be used in the analysis and paper presentation

# save

save(exp_rdf, exp_metrics, exp_csims, exp_bertfeats, altcondnames,
     prompt_rdf, prompt_metrics, prompt_csims, prompt_bertfeats,
     file = "04_exp_dataprep.Rdata")
